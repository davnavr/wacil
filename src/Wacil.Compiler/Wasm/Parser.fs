module Wacil.Compiler.Wasm.Parser

open System
open System.Buffers
open System.Buffers.Binary
open System.Collections.Immutable
open System.IO
open System.Text

open Wacil.Compiler.Helpers
open Wacil.Compiler.Helpers.Collections

open Wacil.Compiler.Wasm.Format

let getValType (value: byte) =
    match LanguagePrimitives.EnumOfValue value with
    | Type.I32 -> ValType.i32
    | Type.I64 -> ValType.i64
    | Type.F32 -> ValType.f32
    | Type.F64 -> ValType.f64
    | Type.V128 -> ValType.v128
    | Type.ExternRef -> ValType.externref
    | Type.FuncRef -> ValType.funcref
    | _ -> failwithf "bad val type 0x%02X" value

[<Sealed>]
type Reader (source: Stream, byteArrayPool: ArrayPool<byte>) =
    let [<Literal>] ContinueMask = 0b1000_0000uy
    let [<Literal>] SignMask = 0b0100_0000uy

    let mutable offset = 0

    new (source: Stream) = Reader(source, ArrayPool.Shared)

    member _.Offset = offset

    member _.Read(buffer: Span<byte>) =
        let count = source.Read buffer
        offset <- offset + count
        count

    member _.ReadAll(buffer: Span<byte>) =
        let read = source.Read buffer
        if read <> buffer.Length then
            sprintf "0x%02X Attempted to read %i bytes but read %i bytes instead" offset buffer.Length read
            |> EndOfStreamException
            |> raise
        offset <- offset + read

    member this.Skip(length: int) =
        let buffer = Span.stackalloc(if length <= 1024 then length else 1024)
        let mutable remaining = length
        while remaining > 0 do
            this.ReadAll buffer
            remaining <- remaining - buffer.Length

    member this.ReadByte() =
        let mutable value = 0uy
        this.ReadAll(Span.ofByRef(&value))
        value

    member private _.VariableIntegerOverflow() =
        sprintf "0x%02X Exceeded maximum value for this LEB128 integer parser" offset
        |> OverflowException
        |> raise

    /// Reads an unsigned variable-length integer in LEB-128 encoding.
    member this.ReadUnsignedInteger() =
        let mutable cont, n, shifted = true, 0UL, 0
        while cont do
            let mutable b = this.ReadByte()

            if shifted = 63 && b > 1uy then
                while b &&& ContinueMask = ContinueMask do
                    b <- this.ReadByte()

                this.VariableIntegerOverflow()

            n <- n ||| (uint64 (b &&& (~~~ContinueMask)) <<< shifted)
            cont <- b &&& ContinueMask = ContinueMask
            if cont then shifted <- Checked.(+) shifted 7
        n

    member inline this.ReadIndex() = (^T : (static member From: uint64 -> 'T) this.ReadUnsignedInteger())

    member this.ReadSignedIntegerOrNegativeZero() =
        let mutable cont, n, shifted = true, 0L, 0
        let mutable result = ValueNone
        while cont do
            let mutable b = this.ReadByte()

            if shifted = 63 && b <> 0uy && b <> 0x7Fuy then
                while b &&& ContinueMask = ContinueMask do
                    b <- this.ReadByte()

                this.VariableIntegerOverflow()

            n <- n ||| int64 (uint64 (b &&& (~~~ContinueMask)) <<< shifted)
            cont <- b &&& ContinueMask = ContinueMask
            shifted <- Checked.(+) shifted 7

            if not cont then
                // Insert the high sign bits
                if shifted < 64 && SignMask &&& b = SignMask then
                    if b &&& 0b0011_1111uy <> 0uy then
                        result <- ValueSome(n ||| (-1L <<< shifted))
                else
                    result <- ValueSome n
        result

    member this.ReadSignedInteger() = ValueOption.defaultValue 0L (this.ReadSignedIntegerOrNegativeZero())

    /// Reads a 32-bit floating-point number in little-endian order.
    member this.ReadFloat32(): single =
        let buffer = Span.stackalloc 4
        this.ReadAll(buffer)
        let mutable value = 0.0f
        let success = BinaryPrimitives.TryReadSingleLittleEndian(Span.readonly buffer, &value)
        assert success
        value

    /// Reads a 32-bit floating-point number in little-endian order.
    member this.ReadFloat64(): double =
        let buffer = Span.stackalloc 8
        this.ReadAll(buffer)
        let mutable value = 0.0
        let success = BinaryPrimitives.TryReadDoubleLittleEndian(Span.readonly buffer, &value)
        assert success
        value

    member this.ReadName(): Name =
        let length = this.ReadUnsignedInteger() |> Checked.int32
        let mutable bufferArray = null
        let buffer =
            if length < 512 then
                Span.stackalloc length
            else
                bufferArray <- byteArrayPool.Rent length
                Span(bufferArray, 0, length)

        this.ReadAll(buffer)

        let name = Encoding.UTF8.GetString(Span.readonly buffer)
        if not(isNull bufferArray) then byteArrayPool.Return bufferArray
        name

    member this.ReadValType() = this.ReadByte() |> getValType

    member this.ReadRefType() =
        match this.ReadValType() with
        | ValType.Ref r -> r
        | bad -> failwithf "%A is not a valid reference type" bad

    member this.ReadBlockType() =
        match this.ReadSignedIntegerOrNegativeZero() with
        | ValueNone -> BlockType.Void
        | ValueSome value when value >= 0L -> BlockType.Index(TypeIdx.From value)
        | ValueSome value ->
            int8 value &&& 0b0111_1111y
            |> uint8
            |> getValType
            |> BlockType.Val

    member this.ReadResultType(): ResultType =
        let count = this.ReadUnsignedInteger()
        if count > 0UL then
            let types = Array.zeroCreate(Checked.int32 count)
            for i = 0 to types.Length - 1 do types[i] <- this.ReadValType()
            Unsafe.Array.toImmutable types
        else ImmutableArray.Empty

    member this.ReadFuncType() =
        let b = this.ReadByte()
        if b <> 0x60uy then
            failwithf "0x%02X does not mark the start of a function type" b
        { FuncType.Parameters = this.ReadResultType()
          FuncType.Results = this.ReadResultType() }

    member this.ReadLimits() =
        match this.ReadByte() with
        | 0uy -> Limits.ofMin(this.ReadUnsignedInteger() |> Checked.uint32)
        | 1uy ->
            let minimum = this.ReadUnsignedInteger() |> Checked.uint32
            let maximum = this.ReadUnsignedInteger() |> Checked.uint32
            match Limits.tryWithMax minimum (ValueSome maximum) with
            | ValueSome(limits) -> limits
            | ValueNone -> failwith "maximum of limit must be less than minimum"
        | bad -> failwithf "bad limit kind 0x%02X" bad

    member this.ReadTableType() =
        { TableType.ElementType = this.ReadRefType()
          TableType.Limits = this.ReadLimits() }

    member this.ReadGlobalType() =
        { GlobalType.Type = this.ReadValType()
          GlobalType.Mutability =
            match this.ReadByte() with
            | 0uy -> Mutability.Const
            | 1uy -> Mutability.Var
            | bad -> failwithf "0x%02X is not a valid global mutability value" bad }

[<Sealed>]
type InvalidMagicException (actual: ImmutableArray<byte>) =
    inherit Exception("Not a WebAssembly module")

    member _.Magic = actual

[<Sealed>]
type ParseException (offset: int, inner: exn) =
    inherit Exception(sprintf "Exception occured while parsing at offset 0x%04X" offset, inner)

    member _.Offset = offset

let parseMemArg (reader: Reader) =
    let flags = reader.ReadUnsignedInteger() |> Checked.uint8
    let offset = reader.ReadUnsignedInteger() |> Checked.uint32
    let alignment, memory =
        if flags < 64uy then
            MemArgAlign flags, MemIdx.Zero
        else
            MemArgAlign(flags - 64uy), reader.ReadIndex()

    { MemArg.Alignment = alignment
      MemArg.Offset = offset
      MemArg.Memory = memory }

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type BlockBuilderState =
    | Finish
    | Block of BlockType
    | Loop of BlockType
    | If of BlockType
    | IfElse of BlockType * ImmutableArray<Instruction>

[<Struct; NoComparison; NoEquality>]
type BlockBuilder =
    { mutable Instructions: ArrayBuilder<Instruction>
      State: BlockBuilderState }

[<Struct; NoComparison; NoEquality>]
type InstructionBuilderCache =
    { mutable Builders: ArrayBuilder<ArrayBuilder<Instruction>> }

    member this.Rent() =
        let mutable buffer = Unchecked.defaultof<ArrayBuilder<Instruction>>
        if this.Builders.TryPop(&buffer) then
            buffer.Clear()
        else
            buffer <- ArrayBuilder<Instruction>.Create()
        buffer

    member this.Return buffer = this.Builders.Add buffer

let parseExpression (reader: Reader) (instructions: byref<ArrayBuilder<Instruction>>): Expression =
    let mutable nestedBlockLevel = 0

    instructions.Clear()

    while nestedBlockLevel > -1 do
        match LanguagePrimitives.EnumOfValue(reader.ReadByte()) with
        | Opcode.Unreachable -> instructions.Add Unreachable
        | Opcode.Nop -> instructions.Add Nop
        | Opcode.Block ->
            nestedBlockLevel <- Checked.(+) nestedBlockLevel 1
            instructions.Add(Block(reader.ReadBlockType()))
        | Opcode.Loop ->
            nestedBlockLevel <- Checked.(+) nestedBlockLevel 1
            instructions.Add(Loop(reader.ReadBlockType()))
        | Opcode.If ->
            nestedBlockLevel <- Checked.(+) nestedBlockLevel 1
            instructions.Add(If(reader.ReadBlockType()))
        | Opcode.Else -> instructions.Add Else
        | Opcode.End ->
            nestedBlockLevel <- Checked.(-) nestedBlockLevel 1
            instructions.Add End
        | Opcode.Br -> instructions.Add(reader.ReadIndex() |> Br)
        | Opcode.BrIf -> instructions.Add(reader.ReadIndex() |> BrIf)
        | Opcode.BrTable ->
            let mutable targetLabels = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
            for i = 0 to targetLabels.Length - 1 do targetLabels[i] <- reader.ReadIndex()
            let defaultLabel = reader.ReadIndex()
            instructions.Add(BrTable(Unsafe.Array.toImmutable targetLabels, defaultLabel))
        | Opcode.Return -> instructions.Add Return
        | Opcode.Call -> instructions.Add(reader.ReadIndex() |> Call)
        | Opcode.CallIndirect -> instructions.Add(CallIndirect(reader.ReadIndex(), reader.ReadIndex()))
        | Opcode.Drop -> instructions.Add Drop
        | Opcode.Select -> instructions.Add Select
        | Opcode.SelectMany -> raise(NotSupportedException "select instruction that uses multiple values not yet suppoprted")
        | Opcode.LocalGet -> instructions.Add(reader.ReadIndex() |> LocalGet)
        | Opcode.LocalSet -> instructions.Add(reader.ReadIndex() |> LocalSet)
        | Opcode.LocalTee -> instructions.Add(reader.ReadIndex() |> LocalTee)
        | Opcode.GlobalGet -> instructions.Add(reader.ReadIndex() |> GlobalGet)
        | Opcode.GlobalSet -> instructions.Add(reader.ReadIndex() |> GlobalSet)
        | Opcode.TableGet -> instructions.Add(reader.ReadIndex() |> TableGet)
        | Opcode.TableSet -> instructions.Add(reader.ReadIndex() |> TableSet)
        | Opcode.I32Load -> instructions.Add(parseMemArg reader |> I32Load)
        | Opcode.I64Load -> instructions.Add(parseMemArg reader |> I64Load)
        | Opcode.F32Load -> instructions.Add(parseMemArg reader |> F32Load)
        | Opcode.F64Load -> instructions.Add(parseMemArg reader |> F64Load)
        | Opcode.I32Load8S -> instructions.Add(parseMemArg reader |> I32Load8S)
        | Opcode.I32Load8U -> instructions.Add(parseMemArg reader |> I32Load8U)
        | Opcode.I32Load16S -> instructions.Add(parseMemArg reader |> I32Load16S)
        | Opcode.I32Load16U -> instructions.Add(parseMemArg reader |> I32Load16U)
        | Opcode.I64Load8S -> instructions.Add(parseMemArg reader |> I64Load8S)
        | Opcode.I64Load8U -> instructions.Add(parseMemArg reader |> I64Load8U)
        | Opcode.I64Load16S -> instructions.Add(parseMemArg reader |> I64Load16S)
        | Opcode.I64Load16U -> instructions.Add(parseMemArg reader |> I64Load16U)
        | Opcode.I64Load32S -> instructions.Add(parseMemArg reader |> I64Load32S)
        | Opcode.I64Load32U -> instructions.Add(parseMemArg reader |> I64Load32U)
        | Opcode.I32Store -> instructions.Add(parseMemArg reader |> I32Store)
        | Opcode.I64Store -> instructions.Add(parseMemArg reader |> I64Store)
        | Opcode.F32Store -> instructions.Add(parseMemArg reader |> F32Store)
        | Opcode.F64Store -> instructions.Add(parseMemArg reader |> F64Store)
        | Opcode.I32Store8 -> instructions.Add(parseMemArg reader |> I32Store8)
        | Opcode.I32Store16 -> instructions.Add(parseMemArg reader |> I32Store16)
        | Opcode.I64Store8 -> instructions.Add(parseMemArg reader |> I64Store8)
        | Opcode.I64Store16 -> instructions.Add(parseMemArg reader |> I64Store16)
        | Opcode.I64Store32 -> instructions.Add(parseMemArg reader |> I64Store32)
        | Opcode.MemorySize -> instructions.Add(reader.ReadIndex() |> MemorySize)
        | Opcode.MemoryGrow -> instructions.Add(reader.ReadIndex() |> MemoryGrow)
        | Opcode.I32Const -> instructions.Add(reader.ReadSignedInteger() |> Checked.int32 |> I32Const)
        | Opcode.I64Const -> instructions.Add(reader.ReadSignedInteger() |> I64Const)
        | Opcode.F32Const -> instructions.Add(reader.ReadFloat32() |> F32Const)
        | Opcode.F64Const -> instructions.Add(reader.ReadFloat64() |> F64Const)
        | Opcode.I32Eqz -> instructions.Add I32Eqz
        | Opcode.I32Eq -> instructions.Add I32Eq
        | Opcode.I32Ne -> instructions.Add I32Ne
        | Opcode.I32LtS -> instructions.Add I32LtS
        | Opcode.I32LtU -> instructions.Add I32LtU
        | Opcode.I32GtS -> instructions.Add I32GtS
        | Opcode.I32GtU -> instructions.Add I32GtU
        | Opcode.I32LeS -> instructions.Add I32LeS
        | Opcode.I32LeU -> instructions.Add I32LeU
        | Opcode.I32GeS -> instructions.Add I32GeS
        | Opcode.I32GeU -> instructions.Add I32GeU
        | Opcode.I64Eqz -> instructions.Add I64Eqz
        | Opcode.I64Eq -> instructions.Add I64Eq
        | Opcode.I64Ne -> instructions.Add I64Ne
        | Opcode.I64LtS -> instructions.Add I64LtS
        | Opcode.I64LtU -> instructions.Add I64LtU
        | Opcode.I64GtS -> instructions.Add I64GtS
        | Opcode.I64GtU -> instructions.Add I64GtU
        | Opcode.I64LeS -> instructions.Add I64LeS
        | Opcode.I64LeU -> instructions.Add I64LeU
        | Opcode.I64GeS -> instructions.Add I64GeS
        | Opcode.I64GeU -> instructions.Add I64GeU
        | Opcode.F32Eq -> instructions.Add F32Eq
        | Opcode.F32Ne -> instructions.Add F32Ne
        | Opcode.F32Lt -> instructions.Add F32Lt
        | Opcode.F32Gt -> instructions.Add F32Gt
        | Opcode.F32Le -> instructions.Add F32Le
        | Opcode.F32Ge -> instructions.Add F32Ge
        | Opcode.F64Eq -> instructions.Add F64Eq
        | Opcode.F64Ne -> instructions.Add F64Ne
        | Opcode.F64Lt -> instructions.Add F64Lt
        | Opcode.F64Gt -> instructions.Add F64Gt
        | Opcode.F64Le -> instructions.Add F64Le
        | Opcode.F64Ge -> instructions.Add F64Ge
        | Opcode.I32Clz -> instructions.Add I32Clz
        | Opcode.I32Ctz -> instructions.Add I32Ctz
        | Opcode.I32Popcnt -> instructions.Add I32Popcnt
        | Opcode.I32Add -> instructions.Add I32Add
        | Opcode.I32Sub -> instructions.Add I32Sub
        | Opcode.I32Mul -> instructions.Add I32Mul
        | Opcode.I32DivS -> instructions.Add I32DivS
        | Opcode.I32DivU -> instructions.Add I32DivU
        | Opcode.I32RemS -> instructions.Add I32RemS
        | Opcode.I32RemU -> instructions.Add I32RemU
        | Opcode.I32And -> instructions.Add I32And
        | Opcode.I32Or -> instructions.Add I32Or
        | Opcode.I32Xor -> instructions.Add I32Xor
        | Opcode.I32Shl -> instructions.Add I32Shl
        | Opcode.I32ShrS -> instructions.Add I32ShrS
        | Opcode.I32ShrU -> instructions.Add I32ShrU
        | Opcode.I32Rotl -> instructions.Add I32Rotl
        | Opcode.I32Rotr -> instructions.Add I32Rotr
        | Opcode.I64Clz -> instructions.Add I64Clz
        | Opcode.I64Ctz -> instructions.Add I64Ctz
        | Opcode.I64Popcnt -> instructions.Add I64Popcnt
        | Opcode.I64Add -> instructions.Add I64Add
        | Opcode.I64Sub -> instructions.Add I64Sub
        | Opcode.I64Mul -> instructions.Add I64Mul
        | Opcode.I64DivS -> instructions.Add I64DivS
        | Opcode.I64DivU -> instructions.Add I64DivU
        | Opcode.I64RemS -> instructions.Add I64RemS
        | Opcode.I64RemU -> instructions.Add I64RemU
        | Opcode.I64And -> instructions.Add I64And
        | Opcode.I64Or -> instructions.Add I64Or
        | Opcode.I64Xor -> instructions.Add I64Xor
        | Opcode.I64Shl -> instructions.Add I64Shl
        | Opcode.I64ShrS -> instructions.Add I64ShrS
        | Opcode.I64ShrU -> instructions.Add I64ShrU
        | Opcode.I64Rotl -> instructions.Add I64Rotl
        | Opcode.I64Rotr -> instructions.Add I64Rotr
        | Opcode.F32Abs -> instructions.Add F32Abs
        | Opcode.F32Neg -> instructions.Add F32Neg
        | Opcode.F32Ceil -> instructions.Add F32Ceil
        | Opcode.F32Floor -> instructions.Add F32Floor
        | Opcode.F32Trunc -> instructions.Add F32Trunc
        | Opcode.F32Nearest -> instructions.Add F32Nearest
        | Opcode.F32Sqrt -> instructions.Add F32Sqrt
        | Opcode.F32Add -> instructions.Add F32Add
        | Opcode.F32Sub -> instructions.Add F32Sub
        | Opcode.F32Mul -> instructions.Add F32Mul
        | Opcode.F32Div -> instructions.Add F32Div
        | Opcode.F32Min -> instructions.Add F32Min
        | Opcode.F32Max -> instructions.Add F32Max
        | Opcode.F32Copysign -> instructions.Add F32Copysign
        | Opcode.F64Abs -> instructions.Add F64Abs
        | Opcode.F64Neg -> instructions.Add F64Neg
        | Opcode.F64Ceil -> instructions.Add F64Ceil
        | Opcode.F64Floor -> instructions.Add F64Floor
        | Opcode.F64Trunc -> instructions.Add F64Trunc
        | Opcode.F64Nearest -> instructions.Add F64Nearest
        | Opcode.F64Sqrt -> instructions.Add F64Sqrt
        | Opcode.F64Add -> instructions.Add F64Add
        | Opcode.F64Sub -> instructions.Add F64Sub
        | Opcode.F64Mul -> instructions.Add F64Mul
        | Opcode.F64Div -> instructions.Add F64Div
        | Opcode.F64Min -> instructions.Add F64Min
        | Opcode.F64Max -> instructions.Add F64Max
        | Opcode.F64Copysign -> instructions.Add F64Copysign
        | Opcode.I32WrapI64 -> instructions.Add I32WrapI64
        | Opcode.I32TruncF32S -> instructions.Add I32TruncF32S
        | Opcode.I32TruncF32U -> instructions.Add I32TruncF32U
        | Opcode.I32TruncF64S -> instructions.Add I32TruncF64S
        | Opcode.I32TruncF64U -> instructions.Add I32TruncF64U
        | Opcode.I64ExtendI32S -> instructions.Add I64ExtendI32S
        | Opcode.I64ExtendI32U -> instructions.Add I64ExtendI32U
        | Opcode.I64TruncF32S -> instructions.Add I64TruncF32S
        | Opcode.I64TruncF32U -> instructions.Add I64TruncF32U
        | Opcode.I64TruncF64S -> instructions.Add I64TruncF64S
        | Opcode.I64TruncF64U -> instructions.Add I64TruncF64U
        | Opcode.F32ConvertI32S -> instructions.Add F32ConvertI32S
        | Opcode.F32ConvertI32U -> instructions.Add F32ConvertI32U
        | Opcode.F32ConvertI64S -> instructions.Add F32ConvertI64S
        | Opcode.F32ConvertI64U -> instructions.Add F32ConvertI64U
        | Opcode.F32DemoteF64 -> instructions.Add F32DemoteF64
        | Opcode.F64ConvertI32S -> instructions.Add F64ConvertI32S
        | Opcode.F64ConvertI32U -> instructions.Add F64ConvertI32U
        | Opcode.F64ConvertI64S -> instructions.Add F64ConvertI64S
        | Opcode.F64ConvertI64U -> instructions.Add F64ConvertI64U
        | Opcode.F64PromoteF32 -> instructions.Add F64PromoteF32
        | Opcode.I32ReinterpretF32 -> instructions.Add I32ReinterpretF32
        | Opcode.I64ReinterpretF64 -> instructions.Add I64ReinterpretF64
        | Opcode.F32ReinterpretI32 -> instructions.Add F32ReinterpretI32
        | Opcode.F64ReinterpretI64 -> instructions.Add F64ReinterpretI64
        | Opcode.I32Extend8S -> instructions.Add I32Extend8S
        | Opcode.I32Extend16S -> instructions.Add I32Extend16S
        | Opcode.I64Extend8S -> instructions.Add I64Extend8S
        | Opcode.I64Extend32S -> instructions.Add I64Extend32S
        | Opcode.I64Extend16S -> instructions.Add I64Extend16S
        | Opcode.RefNull ->
            match reader.ReadValType() with
            | ValType.Ref r -> instructions.Add(RefNull r)
            | bad -> failwithf "%A is not a ref type" bad
        | Opcode.RefIsNull -> instructions.Add RefIsNull
        | Opcode.RefFunc -> instructions.Add(reader.ReadIndex() |> RefFunc)
        | Opcode.PrefixFC ->
            match reader.ReadUnsignedInteger() with
            | 0UL -> instructions.Add I32TruncSatF32S
            | 1UL -> instructions.Add I32TruncSatF32U
            | 2UL -> instructions.Add I32TruncSatF64S
            | 3UL -> instructions.Add I32TruncSatF64U
            | 4UL -> instructions.Add I64TruncSatF32S
            | 5UL -> instructions.Add I64TruncSatF32U
            | 6UL -> instructions.Add I64TruncSatF64S
            | 7UL -> instructions.Add I64TruncSatF64U
            | 8UL -> instructions.Add(MemoryInit(reader.ReadIndex(), reader.ReadIndex()))
            | 9UL -> instructions.Add(reader.ReadIndex() |> DataDrop)
            | 10UL -> instructions.Add(MemoryCopy(reader.ReadIndex(), reader.ReadIndex()))
            | 11UL -> instructions.Add(reader.ReadIndex() |> MemoryFill)
            | 12UL -> instructions.Add(TableInit(reader.ReadIndex(), reader.ReadIndex()))
            | 13UL -> instructions.Add(reader.ReadIndex() |> ElemDrop)
            | 14UL -> instructions.Add(TableCopy(reader.ReadIndex(), reader.ReadIndex()))
            | 15UL -> instructions.Add(reader.ReadIndex() |> TableGrow)
            | 16UL -> instructions.Add(reader.ReadIndex() |> TableSize)
            | 17UL -> instructions.Add(reader.ReadIndex() |> TableFill)
            | bad -> failwithf "Invalid prefixed instruction 0xFC 0x%02X" bad
        | Opcode.PrefixFD ->
            match reader.ReadUnsignedInteger() with
            | bad -> failwithf "Invalid vector instruction 0xFD 0x%02X" bad
        | bad -> failwithf "0x%02X is not a valid opcode" (uint8 bad)

    instructions.CopyToImmutableArray()

let parseCodeEntry (reader: Reader) (instructions: byref<ArrayBuilder<_>>) =
    let expectedFunctionSize = reader.ReadUnsignedInteger() |> Checked.int32
    let functionStartOffset = reader.Offset

    let locals = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
    for i = 0 to locals.Length - 1 do
        locals[i] <-
            { Local.Count = reader.ReadUnsignedInteger() |> Checked.uint32
              Local.Type = reader.ReadValType() }

    let code = { Code.Locals = Unsafe.Array.toImmutable locals; Body = parseExpression reader &instructions }

    let actualFunctionSize = reader.Offset - functionStartOffset
    if actualFunctionSize <> expectedFunctionSize then
        failwithf "TODO: Expected code function to haze a size of %i bytes, but got %i" expectedFunctionSize actualFunctionSize

    code

let parseExpressionVec (reader: Reader) (instructions: byref<_>) =
    let mutable expressions = Array.zeroCreate<Expression>(reader.ReadUnsignedInteger() |> Checked.int32)
    for i = 0 to expressions.Length - 1 do
        expressions[i] <- parseExpression reader &instructions
    Unsafe.Array.toImmutable expressions

let parseFunctionIndicesAsExpressions (reader: Reader) =
    let mutable elements = Array.zeroCreate<Expression>(reader.ReadUnsignedInteger() |> Checked.int32)
    for i = 0 to elements.Length - 1 do
        elements[i] <- ImmutableArray.Create(RefFunc(reader.ReadIndex()), End)
    Unsafe.Array.toImmutable elements

let parseFromStream (stream: Stream) =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanRead then invalidArg (nameof stream) "The stream must support reading"

        let reader = new Reader(stream)
        try
            let magicNumberBuffer = Span.stackalloc 4

            reader.ReadAll(magicNumberBuffer)
            if not(Span.equals (Span.readonly magicNumberBuffer) (Preamble.magic.AsSpan())) then
                magicNumberBuffer.ToArray()
                |> Unsafe.Array.toImmutable
                |> InvalidMagicException
                |> raise

            reader.ReadAll(magicNumberBuffer)
            if not(Span.equals (Span.readonly magicNumberBuffer) (Preamble.version.AsSpan())) then
                failwithf "Invalid WebAssembly format version"

            let mutable sections = ArrayBuilder<Section>.Create()
            let mutable instructionSequenceBuilder = ArrayBuilder<_>.Create()
            let sectionTagBuffer = magicNumberBuffer.Slice(0, 1)
            //use sectionContentBuffer = new MemoryStream();
            //let sectionContentReader = Reader(sectionContentBuffer, ArrayPool.Shared)

            while reader.Read sectionTagBuffer > 0 do
                let size = reader.ReadUnsignedInteger() |> Checked.int32
                let sectionStartOffset = reader.Offset

                //sectionContentBuffer.Capacity <- size
                //sectionContentBuffer.Seek(0, SeekOrigin.Begin) |> ignore

                match LanguagePrimitives.EnumOfValue(sectionTagBuffer[0]) with
                | SectionId.Custom ->
                    let custom =
                        { Custom.Name = reader.ReadName()
                          Custom.Contents =
                            let contents = Array.zeroCreate(size - (reader.Offset - sectionStartOffset))
                            reader.ReadAll(Span(contents))
                            Unsafe.Array.toImmutable contents }
                    sections.Add(Section.Custom custom)
                | SectionId.Type ->
                    let types = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                    for i = 0 to types.Length - 1 do types[i] <- reader.ReadFuncType()
                    sections.Add(Section.Type(Unsafe.Array.toImmutable types))
                | SectionId.Import ->
                    let imports = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                    for i = 0 to imports.Length - 1 do
                        imports[i] <-
                            { Import.Module = reader.ReadName()
                              Import.Name = reader.ReadName()
                              Import.Description =
                                match reader.ReadByte() with
                                | 0uy -> reader.ReadIndex() |> ImportDesc.Func
                                | 1uy -> reader.ReadTableType() |> ImportDesc.Table
                                | 2uy -> reader.ReadLimits() |> ImportDesc.Mem
                                | 3uy -> reader.ReadGlobalType() |> ImportDesc.Global
                                | bad -> failwithf "0x%02X is not a valid import descriptor" bad}
                    sections.Add(Section.Import(Unsafe.Array.toImmutable imports))
                | SectionId.Function ->
                    let indices = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                    for i = 0 to indices.Length - 1 do indices[i] <- reader.ReadIndex()
                    sections.Add(Section.Function(Unsafe.Array.toImmutable indices))
                | SectionId.Table ->
                    let tables = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                    for i = 0 to tables.Length - 1 do tables[i] <- reader.ReadTableType()
                    sections.Add(Section.Table(Unsafe.Array.toImmutable tables))
                | SectionId.Memory ->
                    let mems = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                    for i = 0 to mems.Length - 1 do mems[i] <- reader.ReadLimits()
                    sections.Add(Section.Memory(Unsafe.Array.toImmutable mems))
                | SectionId.Global ->
                    let glbls = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                    for i = 0 to glbls.Length - 1 do
                        glbls[i] <-
                            { Global.Type = reader.ReadGlobalType()
                              Global.Expression = parseExpression reader &instructionSequenceBuilder }
                    sections.Add(Section.Global(Unsafe.Array.toImmutable glbls))
                | SectionId.Export ->
                    let exports = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                    for i = 0 to exports.Length - 1 do
                        exports[i] <-
                            { Export.Name = reader.ReadName()
                              Description =
                                let kind = reader.ReadByte()
                                let index = reader.ReadUnsignedInteger()
                                match kind with
                                | 0uy -> ExportDesc.Func(FuncIdx.From index)
                                | 1uy -> ExportDesc.Table(TableIdx.From index)
                                | 2uy -> ExportDesc.Mem(MemIdx.From index)
                                | 3uy -> ExportDesc.Global(GlobalIdx.From index)
                                | _ -> failwithf "0x%02X is not a valid export kind" kind }
                    sections.Add(Section.Export(Unsafe.Array.toImmutable exports))
                | SectionId.Start -> sections.Add(Section.Start(reader.ReadIndex()))
                | SectionId.Element ->
                    let elements = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                    for i = 0 to elements.Length - 1 do
                        elements[i] <-
                            match reader.ReadUnsignedInteger() |> Checked.uint32 with
                            | 0u ->
                                let offset = parseExpression reader &instructionSequenceBuilder
                                let elements = parseFunctionIndicesAsExpressions reader
                                { Element.Type = FuncRef
                                  Element.Expressions = elements
                                  Element.Mode = ElementMode.Active(TableIdx 0, offset) }
                            | 1u ->
                                match reader.ReadByte() with
                                | 0uy ->
                                    { Element.Type = FuncRef
                                      Element.Expressions = parseFunctionIndicesAsExpressions reader
                                      Element.Mode = ElementMode.Passive }
                                | bad -> failwithf "0x%02X is not a valid element kind" bad
                            | 2u ->
                                let table = reader.ReadIndex()
                                let offset = parseExpression reader &instructionSequenceBuilder
                                match reader.ReadByte() with
                                | 0uy ->
                                    { Element.Type = FuncRef
                                      Element.Expressions = parseFunctionIndicesAsExpressions reader
                                      Element.Mode = ElementMode.Active(table, offset) }
                                | bad -> failwithf "0x%02X is not a valid element kind" bad
                            | 3u ->
                                match reader.ReadByte() with
                                | 0uy ->
                                    { Element.Type = FuncRef
                                      Element.Expressions = parseFunctionIndicesAsExpressions reader
                                      Element.Mode = ElementMode.Declarative }
                                | bad -> failwithf "0x%02X is not a valid element kind" bad
                            | 4u ->
                                let offset = parseExpression reader &instructionSequenceBuilder
                                { Element.Type = FuncRef
                                  Element.Expressions = parseExpressionVec reader &instructionSequenceBuilder
                                  Element.Mode = ElementMode.Active(TableIdx 0, offset) }
                            | 5u ->
                                { Element.Type = reader.ReadRefType()
                                  Element.Expressions = parseExpressionVec reader &instructionSequenceBuilder
                                  Element.Mode = ElementMode.Passive }
                            | 6u ->
                                let table = reader.ReadIndex()
                                let offset = parseExpression reader &instructionSequenceBuilder
                                let etype = reader.ReadRefType()
                                { Element.Type = etype
                                  Element.Expressions = parseExpressionVec reader &instructionSequenceBuilder
                                  Element.Mode = ElementMode.Active(table, offset) }
                            | 7u ->
                                { Element.Type = reader.ReadRefType()
                                  Element.Expressions = parseExpressionVec reader &instructionSequenceBuilder
                                  Element.Mode = ElementMode.Declarative }
                            | bad -> failwithf "%i is not a valid element flags combination" bad
                    sections.Add(Section.Element(Unsafe.Array.toImmutable elements))
                | SectionId.Code ->
                    let code = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                    for i = 0 to code.Length - 1 do code[i] <- parseCodeEntry reader &instructionSequenceBuilder
                    sections.Add(Section.Code(Unsafe.Array.toImmutable code))
                | SectionId.Data ->
                    let data = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                    for i = 0 to data.Length - 1 do
                        data[i] <-
                            match reader.ReadUnsignedInteger() |> Checked.uint32 with
                            | 0u ->
                                let expression = parseExpression reader &instructionSequenceBuilder
                                let bytes = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                                reader.ReadAll(Span bytes)
                                { Data.Bytes = Unsafe.Array.toImmutable bytes; Mode = DataMode.Active(MemIdx 0, expression) }
                            | 1u ->
                                let bytes = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                                reader.ReadAll(Span bytes)
                                { Data.Bytes = Unsafe.Array.toImmutable bytes; Mode = DataMode.Passive }
                            | 2u ->
                                let index = reader.ReadIndex()
                                let expression = parseExpression reader &instructionSequenceBuilder
                                let bytes = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                                reader.ReadAll(Span bytes)
                                { Data.Bytes = Unsafe.Array.toImmutable bytes; Mode = DataMode.Active(index, expression) }
                            | bad -> failwithf "0x%02X is not a valid data kind" bad
                    sections.Add(Section.Data(Unsafe.Array.toImmutable data))
                | SectionId.DataCount -> sections.Add(Section.DataCount(reader.ReadUnsignedInteger() |> Checked.uint32))
                | unknown -> failwithf "unknown section id 0x%02X" (uint8 unknown)

                let actualSectionSize = reader.Offset - sectionStartOffset
                //assert (int64 actualSectionSize = sectionContentBuffer.Length)
                if actualSectionSize <> size then
                    failwithf "expected %A section to contain 0x%02X bytes, but got 0x%02X bytes" id size actualSectionSize

            sections.ToImmutableArray()
        with
        | ex -> raise(ParseException(reader.Offset, ex))
    finally
        stream.Close()

let parseFromPath path = parseFromStream(File.OpenRead path)
