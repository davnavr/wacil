namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

using System;
using System.IO;
using System.Text;

/// <summary>
/// Provides a stream that interprets data received from WASI code as a <see cref="String"/> in a particular <see cref="Encoding"/>.
/// </summary>
public sealed class TextOutputStream : Stream {
    private readonly TextWriter output;

    /// <summary>Gets the <see cref="Encoding"/> used to translate data.</summary>
    public Encoding Encoding { get; }

    /// <summary>
    /// Initializes a <see cref="TextOutputStream"/> instance with the specified <paramref name="encoding"/> and the
    /// <see cref="TextWriter"/> to write to.
    /// </summary>
    public TextOutputStream(TextWriter output, Encoding encoding) {
        ArgumentNullException.ThrowIfNull(output);
        ArgumentNullException.ThrowIfNull(encoding);
        this.output = output;
        Encoding = encoding;
    }

    /// <summary>
    /// Initializes a <see cref="TextOutputStream"/> with a <see cref="TextWriter"/> that decoded UTF-8 strings are written to.
    /// </summary>
    public TextOutputStream(TextWriter output) : this(output, Encoding.UTF8) { }

    /// <inheritdoc/>
    public override bool CanWrite { get => true; }

    /// <inheritdoc/>
    public override void Write(ReadOnlySpan<byte> buffer) {
        output.Write(Encoding.GetString(buffer));
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
