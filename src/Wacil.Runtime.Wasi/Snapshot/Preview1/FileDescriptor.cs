namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

using System.IO;

/// <summary>Represents a WASI file descriptor.</summary>
public sealed class FileDescriptor {
    /// <summary>Gets the underlying <see cref="System.IO.Stream"/>.</summary>
    public Stream Stream { get; init; }

    /// <summary>Initializes a <see cref="FileDescriptor"/> from the specified <paramref name="stream"/>.</summary>
    public FileDescriptor(Stream stream) {
        Stream = stream;
    }

    /// <summary>A file descriptor backed by <see cref="System.IO.Stream.Null"/>.</summary>
    public static FileDescriptor Null { get; } = new FileDescriptor(Stream.Null);

    /// Gets a value indicating whether the file descriptor can be written to.
    public bool CanWrite() => Stream.CanWrite;
}
