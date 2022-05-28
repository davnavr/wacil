module Wacil.Compiler.Wasm.Parser

open System
open System.Buffers
open System.Collections.Immutable
open System.IO
open System.Text

open Wacil.Compiler.Helpers
open Wacil.Compiler.Helpers.Collections

open Wacil.Compiler.Wasm.Format

[<Sealed>]
type Reader (source: Stream, byteArrayPool: ArrayPool<byte>) =
    let [<Literal>] ContinueMask = 0b1000_0000uy

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

    member this.ReadSignedInteger() =
        let mutable cont, n, shifted = true, 0L, 0
        while cont do
            let mutable b = this.ReadByte()

            if shifted = 63 && b <> 0uy && b <> 0x7Fuy then
                while b &&& ContinueMask = ContinueMask do
                    b <- this.ReadByte()

                this.VariableIntegerOverflow()

            n <- n ||| int64 (uint64 (b &&& (~~~ContinueMask)) <<< shifted)
            cont <- b &&& ContinueMask = ContinueMask
            shifted <- Checked.(+) shifted 7

            // Insert the high sign bits
            if not cont && shifted < 64 && 0b0100_0000uy &&& b <> 0uy then
                n <- -1L <<< shifted
        n

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

    member this.ReadValType() =
        match LanguagePrimitives.EnumOfValue(this.ReadByte()) with
        | Type.I32 -> ValType.Num I32
        | Type.I64 -> ValType.Num I64
        | Type.F32 -> ValType.Num F32
        | Type.F64 -> ValType.Num F64
        | Type.V128 -> ValType.Vec V128
        | Type.ExternRef -> ValType.Ref ExternRef
        | Type.FuncRef -> ValType.Ref FuncRef
        | bad -> failwithf "bad val type 0x%02X" (uint8 bad)

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

let parseExpression (reader: Reader): Expression =
    let mutable body = ArrayBuilder<Instruction>.Create()
    let mutable expectedBlockEnds = 1u
    //let mutable nestedBlockInstructions = Stack<ArrayBuilder>
    while expectedBlockEnds > 0u do
        match LanguagePrimitives.EnumOfValue(reader.ReadByte()) with
        | Opcode.Nop -> body.Add Instruction.Nop
        | Opcode.Unreachable -> body.Add Instruction.Unreachable
        | Opcode.End -> expectedBlockEnds <- Checked.(-) expectedBlockEnds 1u
        | bad -> failwithf "0x%02X is not a valid opcode" (uint8 bad)
    body.ToImmutableArray() |> Expr

let parseCodeEntry (reader: Reader) =
    let expectedFunctionSize = reader.ReadUnsignedInteger() |> Checked.int32
    let functionStartOffset = reader.Offset

    let locals = Array.zeroCreate(reader.ReadUnsignedInteger() |> Checked.int32)
    for i = 0 to locals.Length - 1 do
        locals[i] <-
            { Local.Count = reader.ReadUnsignedInteger() |> Checked.uint32
              Local.Type = reader.ReadValType() }

    let code = { Code.Locals = Unsafe.Array.toImmutable locals; Body = parseExpression reader }

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
                for i = 0 to code.Length - 1 do code[i] <- parseCodeEntry reader
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
