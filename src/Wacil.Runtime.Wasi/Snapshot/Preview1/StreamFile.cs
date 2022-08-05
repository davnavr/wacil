namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

using System;
using System.IO;

/// <summary>Provides a WASI file descriptor implementation backed by a <see cref="Stream"/>.</summary>
public sealed class StreamFile : IFile {
    private readonly Stream stream;

    /// <summary>Initializes a new <see cref="StreamFile"/> instance with the specified <paramref name="stream"/>.</summary>
    public StreamFile(Stream stream) {
        ArgumentNullException.ThrowIfNull(stream);
        this.stream = stream;
    }

    /// <inheritdoc/>
    public bool CanRead { get => stream.CanRead; }

    /// <inheritdoc/>
    public bool CanWrite { get => stream.CanWrite; }

    /// <inheritdoc/>
    public bool CanSeek { get => stream.CanSeek; }

    /// <inheritdoc/>
    public void Write(ReadOnlySpan<byte> data) => stream.Write(data);

    /// <inheritdoc/>
    public void Dispose() => stream.Close();
}
