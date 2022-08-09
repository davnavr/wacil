namespace Wacil.Compiler.Helpers.IO

open System

[<Sealed>]
type internal ReadOnlyMemoryStream (bytes: ReadOnlyMemory<byte>) =
    inherit IO.Stream()

    let mutable current = bytes

    new (bytes: Collections.Immutable.ImmutableArray<byte>) =
        new ReadOnlyMemoryStream(bytes.AsMemory())

    override _.CanRead = true
    override _.CanSeek = false
    override _.CanWrite = false

    override _.Read(buffer: Span<byte>) =
        let length = min buffer.Length current.Length
        current.Span.Slice(0, length).CopyTo(buffer)
        current <- current.Slice length
        length

    override this.Read(buffer: byte[], offset, count) = this.Read(Span(buffer, offset, count))

    override _.Seek(offset: int64, origin: IO.SeekOrigin) = raise(NotSupportedException())

    override _.Length = raise(NotSupportedException())

    override _.Position
        with get() = raise(NotSupportedException())
        and set _ = raise(NotSupportedException())

    override _.SetLength(length: int64) = raise(NotSupportedException())

    override _.Write(buffer: byte[], offset: int, count: int) = raise(NotSupportedException())

    override _.Flush() = ()
