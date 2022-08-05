namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

using System;
using System.IO;
using System.Text;

/// <summary>
/// Provides a stream that interprets data received from WASI code as a <see cref="String"/>, with decoding being done by a <see cref="Decoder"/>.
/// </summary>
public sealed class TextOutputStream : Stream {
    private readonly TextWriter output;

    private readonly char[] buffer = new char[64];

    /// <summary>Gets the <see cref="Decoder"/> used to decode data into a <see cref="String"/>.</summary>
    public Decoder Decoder { get; }

    /// <summary>
    /// Initializes a <see cref="TextOutputStream"/> instance with the specified <paramref name="decoder"/> and the
    /// <see cref="TextWriter"/> to write strings to.
    /// </summary>
    public TextOutputStream(TextWriter output, Decoder decoder) {
        this.output = output;
        Decoder = decoder;
    }

    /// <summary>
    /// Initializes a <see cref="TextOutputStream"/> instance with the specified <paramref name="encoding"/> and the
    /// <see cref="TextWriter"/> to write to.
    /// </summary>
    public TextOutputStream(TextWriter output, Encoding encoding) : this(output, encoding.GetDecoder()) { }

    /// <summary>
    /// Initializes a <see cref="TextOutputStream"/> with a <see cref="TextWriter"/> that decoded UTF-8 strings are written to.
    /// </summary>
    public TextOutputStream(TextWriter output) : this(output, Encoding.UTF8) { }

    /// <inheritdoc/>
    public override bool CanWrite { get => true; }

    /// <inheritdoc/>
    public override void Write(ReadOnlySpan<byte> buffer) {
        bool _completed = false;
        int bytesUsed, charsUsed;
        while (!buffer.IsEmpty) {
            Decoder.Convert(buffer, this.buffer, false, out bytesUsed, out charsUsed, out _completed);
            output.Write(new Span<char>(this.buffer, 0, charsUsed));
            buffer = buffer.Slice(bytesUsed);
        }
    }

    /// <inheritdoc/>
    public override void Write(byte[] buffer, int offset, int count) => Write(new ReadOnlySpan<byte>(buffer, offset, count));

    /// <inheritdoc/>
    public override void Flush() {
        output.Flush();
    }

    /// <inheritdoc/>
    public override bool CanRead { get => false; }

    /// <inheritdoc/>
    public override bool CanSeek { get => false; }

    /// <inheritdoc/>
    public override long Position {
        get => throw new NotSupportedException();
        set => throw new NotSupportedException();
    }

    /// <inheritdoc/>
    public override long Length { get => throw new NotSupportedException(); }

    /// <inheritdoc/>
    public override int Read(Span<byte> buffer) => throw new NotSupportedException();

    /// <inheritdoc/>
    public override int Read(byte[] buffer, int offset, int count) => throw new NotSupportedException();

    /// <inheritdoc/>
    public override void SetLength(long value) => throw new NotSupportedException();

    /// <inheritdoc/>
    public override long Seek(long offset, SeekOrigin origin) => throw new NotSupportedException();

    /// <inheritdoc/>
    protected override void Dispose(bool disposing) {
        if (disposing) {
            output.Close();
        }
    }
}
