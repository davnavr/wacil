namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

using System;

/// <summary>Defines methods that implement WASI the file descriptor functions.</summary>
public interface IFile : IDisposable {
    /// <summary>Indicates whether the <c>fd_read</c> function is supported.</summary>
    public virtual bool CanRead { get => false; }

    /// <summary>Indicates whether the <c>fd_write</c> function is supported.</summary>
    public virtual bool CanWrite { get => false; }

    /// <summary>Indicates whether the <c>fd_seek</c> function is supported.</summary>
    public virtual bool CanSeek { get => false; }

    /// <summary>Indicates whether the <c>fd_readdir</c> function is supported.</summary>
    public virtual bool CanReadDirectoryEntries { get => false; }

    /// <summary>Writes the specified <paramref name="data"/> to this file descriptor.</summary>
    /// <remarks>This provides the implementation for <c>fd_write</c>.</remarks>
    public virtual void Write(ReadOnlySpan<byte> data) => throw new NotSupportedException("Cannot write to this file descriptor");
}
