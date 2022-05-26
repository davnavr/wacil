module Wacil.Compiler.Wasm.Parser

open System
open System.Collections.Immutable
open System.IO

open Wacil.Compiler.Helpers
open Wacil.Compiler.Helpers.Collections

[<Sealed>]
type Reader (source: Stream) =
    let [<Literal>] ContinueMask = 0b1000_0000uy

    let mutable offset = 0

    member _.ReadAll(buffer: Span<byte>) =
        let read = source.Read buffer
        if read <> buffer.Length then
            raise (EndOfStreamException(sprintf "Attempted to read %i bytes but read %i bytes instead" buffer.Length read))
        offset <- offset + read

    member this.ReadByte() =
        let mutable value = 0uy
        this.ReadAll(Span.ofByRef(&value))
        value

    member this.ReadUInt64() =
        let mutable cont, n, shifted = true, LanguagePrimitives.GenericZero, 0
        while cont do
            let b = this.ReadByte()
            cont <- b &&& ContinueMask = ContinueMask
            n <- n ||| (uint64 (b &&& (~~~ContinueMask)) <<< shifted)
            if b <> ContinueMask then shifted <- Checked.(+) shifted 7
            if shifted > 63 then
                raise(OverflowException "Exceeded maximum value for this LEB128 integer parser")
        n

[<Sealed>]
type InvalidMagicException (actual: ImmutableArray<byte>) =
    inherit Exception("Not a WebAssembly module")

    member _.Magic = actual

let parseFromStream (stream: Stream): Format.Module =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanRead then invalidArg (nameof stream) "The stream must support reading"

        let reader = Reader(stream)
        let magicNumberBuffer = Span.stackalloc 4
        reader.ReadAll(magicNumberBuffer)

        if not(Span.equals (Span.readonly magicNumberBuffer) (Format.Preamble.magic.AsSpan())) then
            magicNumberBuffer.ToArray()
            |> Unsafe.Array.toImmutable
            |> InvalidMagicException
            |> raise

        // TODO: Read version

        let sections = ArrayBuilder<Format.Section>.Create()

        sections.ToImmutableArray()
    finally
        stream.Close()

let parseFromPath path = parseFromStream(File.OpenRead path)
