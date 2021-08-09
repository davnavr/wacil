module Wasm.Read

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.CompilerServices

open Microsoft.FSharp.NativeInterop

open Wasm.Format

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
        | ReadSectionContents id -> sprintf "reading section contents %A (%i)" id (uint8 id)

type ReadException (offset: uint32, state: State, inner: exn) =
    inherit Exception(sprintf "Error while %O at offset %i (0x04X) from start of file: %s" state offset inner.Message, inner)

    member _.Offset = offset
    member _.State = state

let inline (|ReadException|) (e: ReadException) = struct(e.Offset, e.State, e.InnerException)

let printByteSeq: seq<byte> -> _ = Seq.map (sprintf "0x%02X") >> String.concat " "

exception InvalidMagicException of actual: byte[]
with override this.Message = sprintf "Expected magic %s but got %s" (printByteSeq Preamble.magic) (printByteSeq this.actual)

exception InvalidVersionException of actual: byte[]
with override this.Message = sprintf "Expected version %s but got %s" (printByteSeq Preamble.version) (printByteSeq this.actual)

exception InvalidSectionIdException of id: SectionId
with override this.Message = let id' = uint8 this.id in sprintf "Invalid section ID byte 0x%02X (%i)" id' id'

[<Interface>]
type IReader = abstract Read: buffer: Span<byte> -> int32

let readBytes (source: #IReader) (buffer: Span<byte>) =
    let read = source.Read buffer
    if read <> buffer.Length then
        failwithf "TODO: Error for unexpected end of reader (attempted to read %i bytes but read %i bytes instead)" buffer.Length read

let readMagicBytes source (expected: ImmutableArray<byte>) e =
    let actual = SpanHelpers.stackalloc expected.Length
    readBytes source actual
    if actual.SequenceEqual(expected.AsSpan()) |> not then
        actual.ToArray() |> e |> raise

/// Reads a 32-bit unsigned integer in LEB128 encoding.
let readU32 source =
    failwith "TODO: Read it": uint32

//let readLittleEndianU32 source =
//    let buffer = SpanHelpers.stackalloc sizeof<uint32>
//    readBytes source buffer
//    if not BitConverter.IsLittleEndian then buffer.Reverse()
//    BitConverter.ToUInt32(SpanHelpers.readonly buffer)

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type ByteStreamReader<'Stream when 'Stream :> Stream> (source: 'Stream) =
    interface IReader with
        member _.Read buffer = source.Read buffer

let readWasmMagic source = readMagicBytes source Preamble.magic InvalidMagicException

let readWasmVersion source = readMagicBytes source Preamble.version InvalidVersionException

let readSectionId (source: #IReader) =
    let buffer = SpanHelpers.stackalloc sizeof<uint8>
    match source.Read buffer with
    | 0 -> ValueNone
    | _ -> ValueSome(LanguagePrimitives.EnumOfValue buffer.[0])

let readSectionContents (source: #IReader) (id: SectionId) =
    if id > SectionId.DataCount then raise(InvalidSectionIdException id)
    match readU32 source with
    | 0u -> ValueNone
    | size ->
        failwith "What now?"

let read source (sections: ModuleSections.Builder) state =
    match state with
    | ReadMagic ->
        readWasmMagic source
        ValueSome ReadVersionField
    | ReadVersionField ->
        readWasmVersion source
        ValueSome ReadSectionId
    | ReadSectionId -> ValueOption.map ReadSectionContents (readSectionId source)
    | ReadSectionContents id ->
        ValueOption.iter sections.Add (readSectionContents source id)
        ValueSome ReadSectionId

let rec loop source (sections: ModuleSections.Builder) state =
    match read source sections state with
    | ValueSome next -> loop source sections next
    | ValueNone ->
        let version = SpanHelpers.stackalloc sizeof<uint32>
        Preamble.version.AsSpan().CopyTo version
        if not BitConverter.IsLittleEndian then version.Reverse()
        { WasmModule.Version = BitConverter.ToUInt32(SpanHelpers.readonly version)
          Sections = sections.ToImmutable() }
        |> ValidatedModule.Validated

let fromStream (stream: #Stream) =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanRead then invalidArg (nameof stream) "The stream must support reading"
        let reader = ByteStreamReader stream
        loop reader (ModuleSections.Builder()) State.ReadMagic
    finally
        stream.Close()
