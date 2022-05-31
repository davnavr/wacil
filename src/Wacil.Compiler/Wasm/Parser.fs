module Wacil.Compiler.Wasm.Parser

open System
open System.Buffers
open System.Buffers.Binary
open System.Collections.Immutable
open System.IO
open System.Runtime.CompilerServices
open System.Text

open Wacil.Compiler.Helpers
open Wacil.Compiler.Helpers.Collections

open Wacil.Compiler.Wasm.Format

let getValType (value: byte) =
    match LanguagePrimitives.EnumOfValue value with
    | Type.I32 -> ValType.Num I32
    | Type.I64 -> ValType.Num I64
    | Type.F32 -> ValType.Num F32
    | Type.F64 -> ValType.Num F64
    | Type.V128 -> ValType.Vec V128
    | Type.ExternRef -> ValType.Ref ExternRef
    | Type.FuncRef -> ValType.Ref FuncRef
    | _ -> failwithf "bad val type 0x%02X" value

[<Sealed>]
type Reader (source: Stream, byteArrayPool: ArrayPool<byte>) =
    let [<Literal>] ContinueMask = 0b1000_0000uy
    let [<Literal>] SignMask = 0b0100_0000uy

    let mutable offset = 0

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

    member this.ReadBlockType() =
        match this.ReadSignedIntegerOrNegativeZero() with
        | ValueNone -> BlockType.Void
        | ValueSome value when value >= 0L -> BlockType.Index(Checked.uint32 value)
        | ValueSome value ->
            (int8 value * -1y) ||| 0b0100_0000y
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

[<Sealed>]
type InvalidMagicException (actual: ImmutableArray<byte>) =
    inherit Exception("Not a WebAssembly module")

    member _.Magic = actual

let parseMemArg (reader: Reader) =
    { MemArg.Alignment = reader.ReadUnsignedInteger() |> Checked.uint32 |> MemArgAlignment
      MemArg.Offset = reader.ReadUnsignedInteger() |> Checked.uint32 }

let parseMemoryIndex (reader: Reader) =
    if reader.ReadByte() <> 0uy then
        failwithf "TODO: Expected 0 byte after memory instruction"

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

let parseExpression (reader: Reader) (instructionBuilderCache: byref<InstructionBuilderCache>): Expression =
    let mutable body = ImmutableArray.Empty
    let mutable blockInstructionStack = ArrayBuilder<BlockBuilder>.Create()

    blockInstructionStack.Add
        { BlockBuilder.Instructions = instructionBuilderCache.Rent()
          State = BlockBuilderState.Finish }

    while not blockInstructionStack.IsEmpty do
        let mutable block = &blockInstructionStack.LastRef().Instructions

        match LanguagePrimitives.EnumOfValue(reader.ReadByte()) with
        | Opcode.Unreachable -> block.Add(Instruction.Normal Unreachable)
        | Opcode.Nop -> block.Add(Instruction.Normal Nop)
        | Opcode.Block ->
            blockInstructionStack.Add
                { BlockBuilder.Instructions = instructionBuilderCache.Rent()
                  State = BlockBuilderState.Block(reader.ReadBlockType()) }
        | Opcode.Loop ->
            blockInstructionStack.Add
                { BlockBuilder.Instructions = instructionBuilderCache.Rent()
                  State = BlockBuilderState.Loop(reader.ReadBlockType()) }
        | Opcode.If ->
            blockInstructionStack.Add
                { BlockBuilder.Instructions = instructionBuilderCache.Rent()
                  State = BlockBuilderState.If(reader.ReadBlockType()) }
        //| Opcode.Else ->
        | Opcode.End ->
            let mutable popped = Unchecked.defaultof<BlockBuilder>
            blockInstructionStack.Pop(&popped)
            let instructions = popped.Instructions.ToImmutableArray()

            match popped.State with
            | BlockBuilderState.Finish -> body <- instructions
            | BlockBuilderState.Block ty ->
                let mutable block = &blockInstructionStack.LastRef().Instructions
                block.Add(Instruction.Structured { StructuredInstruction.Kind = Block; Type = ty; Instructions = instructions })
            | BlockBuilderState.Loop ty ->
                let mutable block = &blockInstructionStack.LastRef().Instructions
                block.Add(Instruction.Structured { StructuredInstruction.Kind = Loop; Type = ty; Instructions = instructions })
            | BlockBuilderState.If ty ->
                let mutable block = &blockInstructionStack.LastRef().Instructions
                block.Add(Instruction.Structured { Kind = IfElse ImmutableArray.Empty; Type = ty; Instructions = instructions })
            | BlockBuilderState.IfElse(ty, branch) ->
                let mutable block = &blockInstructionStack.LastRef().Instructions
                block.Add(Instruction.Structured { Kind = IfElse instructions; Type = ty; Instructions = branch })

            instructionBuilderCache.Return popped.Instructions
        | Opcode.Br -> block.Add(reader.ReadUnsignedInteger() |> Checked.uint32 |> Br |> Instruction.Normal)
        | Opcode.Call -> block.Add(reader.ReadUnsignedInteger() |> Checked.uint32 |> Call |> Instruction.Normal)
        | Opcode.Drop -> block.Add(Instruction.Normal Drop)
        | Opcode.LocalGet -> block.Add(reader.ReadUnsignedInteger() |> Checked.uint32 |> LocalGet |> Instruction.Normal)
        | Opcode.LocalSet -> block.Add(reader.ReadUnsignedInteger() |> Checked.uint32 |> LocalSet |> Instruction.Normal)
        | Opcode.LocalTee -> block.Add(reader.ReadUnsignedInteger() |> Checked.uint32 |> LocalTee |> Instruction.Normal)
        | Opcode.I32Load -> block.Add(parseMemArg reader |> I32Load |> Instruction.Normal)
        | Opcode.I64Load -> block.Add(parseMemArg reader |> I64Load |> Instruction.Normal)
        | Opcode.F32Load -> block.Add(parseMemArg reader |> F32Load |> Instruction.Normal)
        | Opcode.F64Load -> block.Add(parseMemArg reader |> F64Load |> Instruction.Normal)
        | Opcode.I32Store -> block.Add(parseMemArg reader |> I32Store |> Instruction.Normal)
        | Opcode.I64Store -> block.Add(parseMemArg reader |> I64Store |> Instruction.Normal)
        | Opcode.F32Store -> block.Add(parseMemArg reader |> F32Store |> Instruction.Normal)
        | Opcode.F64Store -> block.Add(parseMemArg reader |> F64Store |> Instruction.Normal)
        | Opcode.MemoryGrow ->
            parseMemoryIndex reader
            block.Add(Instruction.Normal MemoryGrow)
        | Opcode.I32Const -> block.Add(reader.ReadSignedInteger() |> Checked.int32 |> I32Const |> Instruction.Normal)
        | Opcode.I64Const -> block.Add(reader.ReadSignedInteger() |> I64Const |> Instruction.Normal)
        | Opcode.F32Const -> block.Add(reader.ReadFloat32() |> F32Const |> Instruction.Normal)
        | Opcode.F64Const -> block.Add(reader.ReadFloat64() |> F64Const |> Instruction.Normal)
        | Opcode.I32Eqz -> block.Add(Instruction.Normal I32Eqz)
        | Opcode.I32Eq -> block.Add(Instruction.Normal I32Eq)
        | Opcode.I32Ne -> block.Add(Instruction.Normal I32Ne)
        | Opcode.I32LtS -> block.Add(Instruction.Normal I32LtS)
        | Opcode.I32LtU -> block.Add(Instruction.Normal I32LtU)
        | Opcode.I32GtS -> block.Add(Instruction.Normal I32GtS)
        | Opcode.I32GtU -> block.Add(Instruction.Normal I32GtU)
        | Opcode.I32LeS -> block.Add(Instruction.Normal I32LeS)
        | Opcode.I32LeU -> block.Add(Instruction.Normal I32LeU)
        | Opcode.I32GeS -> block.Add(Instruction.Normal I32GeS)
        | Opcode.I32GeU -> block.Add(Instruction.Normal I32GeU)
        | Opcode.I64Eqz -> block.Add(Instruction.Normal I64Eqz)
        | Opcode.I64Eq -> block.Add(Instruction.Normal I64Eq)
        | Opcode.I64Ne -> block.Add(Instruction.Normal I64Ne)
        | Opcode.I64LtS -> block.Add(Instruction.Normal I64LtS)
        | Opcode.I64LtU -> block.Add(Instruction.Normal I64LtU)
        | Opcode.I64GtS -> block.Add(Instruction.Normal I64GtS)
        | Opcode.I64GtU -> block.Add(Instruction.Normal I64GtU)
        | Opcode.I64LeS -> block.Add(Instruction.Normal I64LeS)
        | Opcode.I64LeU -> block.Add(Instruction.Normal I64LeU)
        | Opcode.I64GeS -> block.Add(Instruction.Normal I64GeS)
        | Opcode.I64GeU -> block.Add(Instruction.Normal I64GeU)
        | Opcode.I32Add -> block.Add(Instruction.Normal I32Add)
        | Opcode.I32Sub -> block.Add(Instruction.Normal I32Sub)
        | Opcode.I32Mul -> block.Add(Instruction.Normal I32Mul)
        | Opcode.I32And -> block.Add(Instruction.Normal I32And)
        | Opcode.I64Sub -> block.Add(Instruction.Normal I64Sub)
        | Opcode.I64Mul -> block.Add(Instruction.Normal I64Mul)
        | bad -> failwithf "0x%02X is not a valid opcode" (uint8 bad)

    body

let parseCodeEntry (reader: Reader) (instructionBuilderCache: byref<InstructionBuilderCache>) =
    let expectedFunctionSize = reader.ReadUnsignedInteger() |> Checked.int32
    let functionStartOffset = reader.Offset

    let locals = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
    for i = 0 to locals.Length - 1 do
        locals[i] <-
            { Local.Count = reader.ReadUnsignedInteger() |> Checked.uint32
              Local.Type = reader.ReadValType() }

    let code =
        { Code.Locals = Unsafe.Array.toImmutable locals
          Body = parseExpression reader &instructionBuilderCache }

    let actualFunctionSize = reader.Offset - functionStartOffset
    if actualFunctionSize <> expectedFunctionSize then
        failwithf "TODO: Expected code function to haze a size of %i bytes, but got %i" expectedFunctionSize actualFunctionSize

    code

let parseFromStream (stream: Stream) =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanRead then invalidArg (nameof stream) "The stream must support reading"

        let reader = Reader(stream, ArrayPool.Shared)
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
        let mutable instructionBuilderCache = { InstructionBuilderCache.Builders = ArrayBuilder<_>.Create() }
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
                let name = reader.ReadName()
                let contents = reader.ReadUnsignedInteger() |> Checked.int32 |> Array.zeroCreate
                reader.ReadAll(Span(contents))
                sections.Add(Section.Custom { Custom.Name = name; Custom.Contents = Unsafe.Array.toImmutable contents })
            | SectionId.Type ->
                let types = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                for i = 0 to types.Length - 1 do types[i] <- reader.ReadFuncType()
                sections.Add(Section.Type(Unsafe.Array.toImmutable types))
            | SectionId.Function ->
                let indices = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                for i = 0 to indices.Length - 1 do indices[i] <- reader.ReadUnsignedInteger() |> Checked.uint32
                sections.Add(Section.Function(Unsafe.Array.toImmutable indices))
            | SectionId.Memory ->
                let mems = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                for i = 0 to mems.Length - 1 do mems[i] <- reader.ReadLimits()
                sections.Add(Section.Memory(Unsafe.Array.toImmutable mems))
            | SectionId.Global ->
                let glbls = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                for i = 0 to glbls.Length - 1 do
                    glbls[i] <-
                        { Global.Type =
                            { GlobalType.Type = reader.ReadValType()
                              Mutability =
                                match reader.ReadByte() with
                                | 0uy -> Mutability.Const
                                | 1uy -> Mutability.Var
                                | bad -> failwithf "0x%02X is not a valid global mutability value" bad }
                          Global.Expression = parseExpression reader &instructionBuilderCache }
                sections.Add(Section.Global(Unsafe.Array.toImmutable glbls))
            | SectionId.Export ->
                let exports = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                for i = 0 to exports.Length - 1 do
                    exports[i] <-
                        { Export.Name = reader.ReadName()
                          Description =
                            let kind = reader.ReadByte()
                            let index: Index = reader.ReadUnsignedInteger() |> Checked.uint32
                            match kind with
                            | 0uy -> ExportDesc.Func index
                            | 1uy -> ExportDesc.Table index
                            | 2uy -> ExportDesc.Mem index
                            | 3uy -> ExportDesc.Global index
                            | _ -> failwithf "0x%02X is not a valid export kind" kind }
                sections.Add(Section.Export(Unsafe.Array.toImmutable exports))
            | SectionId.Start -> sections.Add(Section.Start(reader.ReadUnsignedInteger() |> Checked.uint32))
            | SectionId.Code ->
                let code = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
                for i = 0 to code.Length - 1 do code[i] <- parseCodeEntry reader &instructionBuilderCache
                sections.Add(Section.Code(Unsafe.Array.toImmutable code))
            | unknown -> failwithf "unknown section id 0x%02X" (uint8 unknown)

            let actualSectionSize = reader.Offset - sectionStartOffset
            //assert (int64 actualSectionSize = sectionContentBuffer.Length)
            if actualSectionSize <> size then
                failwithf "expected section to contain 0x%02X bytes, but got 0x%02X bytes" size actualSectionSize

        sections.ToImmutableArray()
    finally
        stream.Close()

let parseFromPath path = parseFromStream(File.OpenRead path)
