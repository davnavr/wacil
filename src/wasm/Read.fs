module Wasm.Read

open System
open System.Collections.Immutable
open System.IO

open Wasm.Format
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
        | ReadSectionContents id -> sprintf "reading section contents of the %A (%i) section" id (uint8 id)

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
        sprintf "The ID byte  0x%02X (%i) of the section at index %i was invalid" id' id' this.index

type ByteStream (source: Stream) =
    let mutable pos = 0u
    member _.Position = pos
    member _.Read buffer =
        let read = source.Read buffer
        pos <- Checked.(+) pos (Checked.uint32 read)
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
    let inline private leb128 size convert (stream: ByteStream) =
        let mutable cont, n, shifted = true, LanguagePrimitives.GenericZero, 0
        while cont do
            let b = readByte stream
            cont <- b &&& 0b1000_0000uy = 0b1000_0000uy
            n <- convert (b &&& 0b0111_1111uy) <<< shifted
            shifted <- Checked.(+) shifted 7
            if shifted > size then failwith "TODO: Error for exceeded max allowed value for this kind of LEB128 integer"
        n

    /// Reads a 32-bit unsigned integer in LEB128 encoding.
    let u32 stream = leb128 32 uint32 stream

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

    let valtype stream =
        let t = readTypeByte stream
        match t with
        | NumType(ValueSome n) -> ValType.NumType n
        //| RefType
        | _ ->
            let t' = uint8 t
            failwithf "TODO: Invalid type byte 0x%02X (%i)" t' t'

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

[<Struct>]
type FunctionReader = interface IReader<Function> with member _.Read stream = { Function.Type = index stream }

let readSectionContents stream i (id: SectionId) =
    match Integer.u32 stream with
    | 0u -> ValueNone
    | size ->
        // TODO: Wrap stream to limit the number of bytes that are read. Or just make a new exception type ___MismatchException and keep track of the number of bytes that are actually read.
        match id with
        | SectionId.Type -> ValueSome(TypeSection(ivector<Type.FuncTypeReader, _, _> stream Index.Zero))
        | SectionId.Function -> ValueSome(FunctionSection(ivector<FunctionReader, _, _> stream Index.Zero))
        | _ -> raise(InvalidSectionIdException(id, i))

let read (stream: ByteStream) (sections: ModuleSections.Builder) state =
    let start = stream.Position
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
            ValueOption.iter sections.Add (readSectionContents stream sections.Count id)
            ValueSome ReadSectionId
    with
    | ex -> raise(ReadException(start, state, ex))

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
