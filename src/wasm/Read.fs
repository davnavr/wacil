module Wasm.ReadModule

open System
open System.Collections.Immutable
open System.IO

open Microsoft.FSharp.Core.Printf

open Wasm.Format
open Wasm.Format.InstructionSet
open Wasm.Format.Types

type State =
    | ReadMagic
    | ReadVersionField
    | ReadSectionId
    | ReadSectionContents of SectionId

    override this.ToString() =
        match this with
        | ReadMagic -> "reading magic number"
        | ReadVersionField -> "reading version field"
        | ReadSectionId -> "reading section identifier"
        | ReadSectionContents id -> sprintf "reading contents of the %A (%i) section" id (uint8 id)

type ReadException (offset: uint32, state: State, inner: exn) =
    inherit Exception(sprintf "Exception while %O at offset %i (0x%04X) from start of file" state offset offset, inner)

    member _.Offset = offset
    member _.State = state

let inline (|ReadException|) (e: ReadException) = struct(e.Offset, e.State, e.InnerException)

let printByteSeq: seq<byte> -> _ = Seq.map (sprintf "0x%02X") >> String.concat " "

exception InvalidMagicException of actual: byte[]
with override this.Message = sprintf "Expected magic %s but got %s" (printByteSeq Preamble.magic) (printByteSeq this.actual)

exception InvalidVersionException of actual: byte[]
with override this.Message = sprintf "Expected version %s but got %s" (printByteSeq Preamble.version) (printByteSeq this.actual)

exception InvalidSectionIdException of id: SectionId * index: int32
with
    override this.Message =
        let id' = uint8 this.id
        sprintf "The ID byte 0x%02X (%i) of the section at index %i was invalid" id' id' this.index

type ByteStream (source: Stream) =
    let mutable pos = 0u
    member _.Stream = source
    member _.Position = pos
    abstract Read: buffer: Span<byte> -> int32
    default _.Read buffer =
        let read = source.Read buffer
        pos <- Checked.(+) pos (Checked.uint32 read)
        read

[<Sealed>]
type SlicedByteStream (length: uint32, source: ByteStream) =
    inherit ByteStream(source.Stream)

    let mutable remaining = length

    member _.Remaining = remaining

    override _.Read buffer =
        let buffer' =
            if uint32 buffer.Length > remaining
            then buffer.Slice(0, Checked.int32 remaining)
            else buffer
        let read = base.Read buffer'
        remaining <- Checked.(-) remaining (uint32 read)
        read

let readAllBytes (stream: ByteStream) (buffer: Span<byte>) =
    let read = stream.Read buffer
    if read <> buffer.Length then
        failwithf "TODO: Error for unexpected end of reader (attempted to read %i bytes but read %i bytes instead)" buffer.Length read

let readByte stream =
    let buffer = SpanHelpers.stackalloc 1
    readAllBytes stream buffer
    buffer.[0]

let readMagicBytes stream (expected: ImmutableArray<byte>) e =
    let actual = SpanHelpers.stackalloc expected.Length
    readAllBytes stream actual
    if actual.SequenceEqual(expected.AsSpan()) |> not then
        actual.ToArray() |> e |> raise

type IReader<'Result> = abstract Read: stream: ByteStream -> 'Result

[<RequireQualifiedAccess>]
module Integer =
    let [<Literal>] private ContinueMask = 0b1000_0000uy

    let inline private uleb128 size convert (stream: ByteStream) =
        let mutable cont, n, shifted = true, LanguagePrimitives.GenericZero, 0
        while cont do
            let b = readByte stream
            cont <- b &&& ContinueMask = ContinueMask
            n <- n ||| (convert (b &&& (~~~ContinueMask)) <<< shifted)
            if b <> ContinueMask then shifted <- Checked.(+) shifted 7
            if shifted > size then failwith "TODO: Error for exceeded max allowed value for this kind of LEB128 integer"
        n

    /// Reads a 32-bit unsigned integer in LEB128 encoding.
    let u32 stream = uleb128 32 uint32 stream

    /// Reads a 64-bit unsigned integer in LEB128 encoding.
    let u64 stream = uleb128 64 uint64 stream

let index<'Class when 'Class :> IndexKinds.Kind> stream = Index<'Class>(Integer.u32 stream)

[<Struct>]
type IndexReader<'Class when 'Class :> IndexKinds.Kind> =
    interface IReader<Index<'Class>> with member _.Read stream = index stream

let readPreambleMagic stream = readMagicBytes stream Preamble.magic InvalidMagicException

let readPreambleVersion stream = readMagicBytes stream Preamble.version InvalidVersionException

let readSectionId (stream: ByteStream) =
    let buffer = SpanHelpers.stackalloc 1
    match stream.Read buffer with
    | 0 -> ValueNone
    | _ -> ValueSome(LanguagePrimitives.EnumOfValue buffer.[0])

let name stream =
    let count = Checked.int32(Integer.u32 stream)
    match count with
    | 0 -> String.Empty
    | _  ->
        let buffer =
            if count <= 512
            then SpanHelpers.stackalloc count
            else Span(Array.zeroCreate count)

        readAllBytes stream buffer

        System.Text.Encoding.UTF8.GetString(SpanHelpers.readonly buffer)

let many<'Reader, 'T when 'Reader : struct and 'Reader :> IReader<'T>> stream count =
    let items = ImmutableArray.CreateBuilder count
    for _ = 0 to count - 1 do items.Add(Unchecked.defaultof<'Reader>.Read stream)
    items

let vector<'Reader, 'T when 'Reader : struct and 'Reader :> IReader<'T>> stream =
    let count = Checked.int32(Integer.u32 stream)
    (many<'Reader, 'T> stream count).ToImmutable()

let ivector<'Reader, 'IndexClass, 'T when 'Reader : struct and 'Reader :> IReader<'T> and 'IndexClass :> IndexKinds.Kind> stream start =
    IndexedVector.ofBlock start (vector<'Reader, 'T> stream): IndexedVector<'IndexClass, _>

[<RequireQualifiedAccess>]
module Type =
    let readTypeByte stream: WasmType = LanguagePrimitives.EnumOfValue(readByte stream)

    let private (|NumType|) t =
        match t with
        | WasmType.i32 -> ValueSome NumType.I32
        | WasmType.i64 -> ValueSome NumType.I64
        | WasmType.f32 -> ValueSome NumType.F32
        | WasmType.f64 -> ValueSome NumType.F64
        | _ -> ValueNone

    let private valtype' t =
        match t with
        | WasmType.i32 -> ValType.NumType NumType.I32
        | WasmType.i64 -> ValType.NumType NumType.I64
        | WasmType.f32 -> ValType.NumType NumType.F32
        | WasmType.f64 -> ValType.NumType NumType.F64
        | WasmType.funcref -> ValType.RefType RefType.FuncRef
        | WasmType.externref -> ValType.RefType RefType.ExternRef
        | _ ->
            let t' = uint8 t
            failwithf "TODO: Invalid type byte 0x%02X (%i)" t' t'

    let valtype stream = valtype'(readTypeByte stream)

    [<Struct>]
    type private ValTypeReader = interface IReader<ValType> with member _.Read stream = valtype stream

    let resulttype stream: ResultType = vector<ValTypeReader, _> stream

    let private funcTypeMagic = ImmutableArray.Create<byte> 0x60uy

    let functype stream =
        readMagicBytes stream funcTypeMagic (fun _ -> failwith "TODO: Make exception type InvalidFunctionType for when byte is not 0x60")
        { FuncType.Parameters = resulttype stream
          Results = resulttype stream }

    [<Struct>]
    type FuncTypeReader = interface IReader<FuncType> with member _.Read stream = functype stream

    let limit<'Reader, 'T when 'Reader : struct and 'Reader :> IReader<'T> and 'T : struct and 'T : equality and 'T : comparison>
        stream
        =
        let inline read() = Unchecked.defaultof<'Reader>.Read stream
        match readByte stream with
        | 0uy -> read() |> Limit.ofMin
        | 1uy ->
            let min = read()
            let max = read()
            match Limit.tryWithMax min (ValueSome max) with
            | ValueSome limit -> limit
            | ValueNone -> failwithf "TODO: Error for limit maximum %A cannot exceed limit minimum %A" max min
        | bad -> failwithf "TODO: Error for invalid limit kind 0x%02X" bad

    [<Struct>]
    type private MemSizeParser =
        interface IReader<MemSize> with
            member _.Read stream =
                let value = Integer.u32 stream
                let multiple = value / PageSize
                if multiple * PageSize = value
                then MemSize(Checked.uint16 multiple)
                else
                    failwithf
                        "TODO: Error for value 0x%08X (%i) is not a multiple of the WebAssembly page size (0x%08X) (%i)"
                        value
                        value
                        PageSize
                        PageSize

    let memtype stream = limit<MemSizeParser, _> stream

    let blocktype stream =
        let sb, index =
            // Need to read signed 33-bit integer, so common LEB128 decoding function not used.
            let mutable cont, n, shifted = true, 0u, 0
            // Empty block type is represented by a negative zero value, so extra variable needed to keep track of sign.
            let mutable sign = 0uy

            while cont do
                let b = readByte stream
                cont <- b &&& 0b1000_0000uy = 0b1000_0000uy
                if not cont then sign <- b

                let mask = if cont then 0b0111_1111uy else 0b0011_1111uy
                n <- n ||| (uint32 (b &&& mask) <<< shifted)

                shifted <- Checked.(+) shifted (if cont then 7 else 6)
                if shifted > 32 then failwith "TODO: Error for exceeded max allowed value for blocktype index"

            sign, int32 n

        if sb &&& 0b0100_0000uy <> 0uy then
            if sb = 0x40uy then
                BlockType.Empty
            elif index >= int32 SByte.MinValue then
                valtype'(LanguagePrimitives.EnumOfValue sb) |> BlockType.ValueType
            else
                failwithf "TODO: Error for invalid blocktype %i" index
        else
            Checked.uint32 index
            |> Index<_>
            |> BlockType.Index

[<Struct>]
type ImportReader =
    interface IReader<Import<ImportDesc>> with
        member _.Read stream =
            { Import.Module = name stream
              Import.Name = name stream
              Description =
                match readByte stream with
                | 0uy -> ImportDesc.Func(index stream)
                //| 1uy -> ImportDesc.Table(Type.tabletype stream)
                | 2uy -> ImportDesc.Mem(Type.memtype stream)
                //| 3uy -> ImportDesc.Global(Type.globaltype stream)
                | bad -> failwithf "TODO: Error for invalid import kind 0x%02X" bad }

[<Struct>]
type FunctionReader = interface IReader<Function> with member _.Read stream = { Function.Type = index stream }

[<Struct>]
type MemReader = interface IReader<Mem> with member _.Read stream = { Mem.Type = Type.memtype stream }

[<Struct>]
type ExportReader =
    interface IReader<Export> with
        member _.Read stream =
            { Export.Name = name stream
              Description =
                match readByte stream with
                | 0uy -> ExportDesc.Func(index stream)
                | 1uy -> ExportDesc.Table(index stream)
                | 2uy -> ExportDesc.Mem(index stream)
                | 3uy -> ExportDesc.Global(index stream)
                | bad -> failwithf "TODO: Error for unknown export kind 0x%02X" bad }

[<Struct>]
type LocalsReader =
    interface IReader<Locals> with
        member _.Read stream = { Locals.Count = Integer.u32 stream; Type = Type.valtype stream }

let memarg stream =
    { MemArg.Alignment = MemArgAlignment.ofInt(Integer.u32 stream)
      Offset = Integer.u32 stream }

let readInstructionSeq (stream: SlicedByteStream) =
    let instructions = ImmutableArray.CreateBuilder()
    let mutable endOpcodeCount = 0u

    while stream.Remaining > 0u do
        let opcode = readByte stream

        match opcode with
        | 0x02uy | 0x03uy | 0x04uy  -> endOpcodeCount <- Checked.(+) endOpcodeCount 1u
        | 0x0Buy when stream.Remaining > 0u -> endOpcodeCount <- Checked.(-) endOpcodeCount 1u
        | _ -> ()

        { Instruction.Opcode = opcode
          Arguments =
            match opcode with
            | 0uy // unreachable
            | 1uy // nop

            | 0x0Buy // end

            | 0x1Auy // drop

            | 0x52uy // i64.ne

            | 0x6Auy

            | 0x7Duy // i64.sub
            | 0x7Euy -> InstructionArguments.Nothing

            | 0x20uy
            | 0x21uy
            | 0x22uy -> InstructionArguments.LocalIndex(index stream)

            | 0x41uy -> int32(Integer.u32 stream) |> InstructionArguments.I32
            | 0x42uy -> int64(Integer.u64 stream) |> InstructionArguments.I64

            | 2uy
            | 3uy
            | 4uy -> InstructionArguments.BlockType(Type.blocktype stream)
            | 5uy ->
                failwith "TODO: Keep track of if and else pairs"
                InstructionArguments.Nothing

            | 0x0Cuy // br
            | 0x0Duy -> InstructionArguments.LabelIndex(index stream)

            | 0x28uy // i32.load
            | 0x29uy // i64.load
            | 0x2Auy // f32.load
            | 0x2Buy // f64.load

            | 0x36uy // i32.store
            | 0x37uy -> InstructionArguments.MemArg(memarg stream)

            | 0x3Fuy // memory.size
            | 0x40uy -> InstructionArguments.MemoryIndex(index stream)

            | bad -> failwithf "TODO: Error for unknown opcode 0x%02X" bad }
        |> instructions.Add

    if endOpcodeCount > 0u then failwith "TODO: Error for mismatch of end opcodes"

    if instructions.Capacity = instructions.Count
    then instructions.MoveToImmutable()
    else instructions.ToImmutable()

let expression stream: Expr =
    let instrs = readInstructionSeq stream
    let terminator = &instrs.ItemRef(instrs.Length - 1)
    if terminator.Opcode <> Control.``end``.Opcode then
        failwithf
            "TODO: Error for last byte of expression was expected to be end opcode 0x%02X but got 0x%02X"
            Control.``end``.Opcode
            terminator.Opcode
    { Expr.Instructions = instrs }

[<Struct>]
type CodeReader =
    interface IReader<Code> with
        member _.Read stream =
            let size = Integer.u32 stream
            let stream' = SlicedByteStream(size, stream)
            let locals = vector<LocalsReader, _> stream'
            { Code.Locals = locals; Body = expression stream' }

let sectionReadError: StringFormat<_> =
    "Exception occured %i bytes from the start of the %A section, with %i bytes remaining unread"

[<Sealed>]
type SectionReadException (offset: uint32, remaining: uint32, id: SectionId, inner: exn) =
    inherit Exception(sprintf sectionReadError offset id remaining, inner)

    /// The offset from the start of the section.
    member _.Offset = offset

    /// The section where the exception during reading occured.
    member _.Id = id

let readSectionContents stream (sections: ModuleSections.Builder) i (id: SectionId) =
    match Integer.u32 stream with
    | 0u -> ValueNone
    | size ->
        let section = SlicedByteStream(size, stream)
        try
            match id with
            | SectionId.Type -> TypeSection(ivector<Type.FuncTypeReader, _, _> section Index.Zero) |> ValueSome
            | SectionId.Import -> ImportSection(ImportSectionContents(vector<ImportReader, _> section)) |> ValueSome
            | SectionId.Function -> FunctionSection(ivector<FunctionReader, _, _> section sections.FunctionStartIndex) |> ValueSome
            //| SectionId.Table -> sections.TableStartIndex
            | SectionId.Memory -> MemorySection(ivector<MemReader, _, _> section sections.MemoryStartIndex) |> ValueSome
            //| SectionId.Global -> sections.GlobalStartIndex
            | SectionId.Export -> ExportSection(vector<ExportReader, _> section) |> ValueSome
            | SectionId.Start -> StartSection(index section) |> ValueSome
            //| SectionId.Element -> 
            | SectionId.Code -> CodeSection(vector<CodeReader, _> section) |> ValueSome

            | SectionId.Custom -> failwith "TODO: Custom sections not supported yet."
            | _ -> raise(InvalidSectionIdException(id, i))
        with
        | ex -> raise(SectionReadException(section.Position, section.Remaining, id, ex))

let read (stream: ByteStream) (sections: ModuleSections.Builder) state =
    try
        match state with
        | ReadMagic ->
            readPreambleMagic stream
            ValueSome ReadVersionField
        | ReadVersionField ->
            readPreambleVersion stream
            ValueSome ReadSectionId
        | ReadSectionId -> ValueOption.map ReadSectionContents (readSectionId stream)
        | ReadSectionContents id ->
            ValueOption.iter sections.Add (readSectionContents stream sections sections.Count id)
            ValueSome ReadSectionId
    with
    | ex -> raise(ReadException(stream.Position, state, ex))

let rec loop stream (sections: ModuleSections.Builder) state =
    match read stream sections state with
    | ValueSome next -> loop stream sections next
    | ValueNone ->
        let version = SpanHelpers.stackalloc sizeof<uint32>
        Preamble.version.AsSpan().CopyTo version
        if not BitConverter.IsLittleEndian then version.Reverse()
        { WasmModule.Version = BitConverter.ToUInt32(SpanHelpers.readonly version)
          Sections = sections.ToImmutable() }
        |> ValidatedModule.Validated

let fromStream (stream: Stream) =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanRead then invalidArg (nameof stream) "The stream must support reading"
        loop (ByteStream stream) (ModuleSections.Builder()) State.ReadMagic
    finally
        stream.Close()

let fromPath path = fromStream(File.OpenRead path)
