namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

using System;

/// <summary>Representing a WASI file descriptor.</summary>
public sealed class FileDescriptor : IDisposable {
    private IFile? file;

    private FileDescriptorRights rights;

    /// <summary>Initializes a new <see cref="FileDescriptor"/>.</summary>
    public FileDescriptor(IFile file) {
        ArgumentNullException.ThrowIfNull(file);
        this.file = file;
    }

    /// <summary>Creates a <see cref="FileDescriptor"/> that supports writing UTF-8 text to the <paramref name="destination"/>.</summary>
    public static FileDescriptor FromOutputStream(TextOutputStream destination) {
        var fd = new FileDescriptor(new StreamFile(destination));
        fd.CanWrite = true;
        return fd;
    }

    /// <summary>Creates a <see cref="FileDescriptor"/> that supports writing UTF-8 text to standard output.</summary>
    public static FileDescriptor FromStandardOutput() => FromOutputStream(new TextOutputStream(Console.Out));

    /// <summary>Creates a <see cref="FileDescriptor"/> that supports writing UTF-8 text to standard error.</summary>
    public static FileDescriptor FromStandardError() => FromOutputStream(new TextOutputStream(Console.Error));

    /// <summary>Gets a value indicating whether this file descriptor is closed.</summary>
    public bool IsClosed { get => file == null; }

    /// <summary>Gets the object that implements this file descriptor's functionality.</summary>
    public IFile File { get => file ?? throw new ObjectDisposedException("file descriptor was closed"); }

    /// <summary>Indicates what operations on this file descriptor are supported and have been explicitly allowed.</summary>
    public FileDescriptorRights Rights { get => rights; }

    /// <summary>Prevents or allows calling <c>fd_write</c>.</summary>
    /// <exception cref="InvalidOperationException">
    /// Thrown if the underlying <see cref="File"/> does not implement <c>fd_write</c>.
    /// </exception>
    public bool CanWrite {
        get => File.CanWrite && (rights & FileDescriptorRights.Write) != FileDescriptorRights.None;
        set {
            if (!File.CanWrite) {
                throw new InvalidOperationException(File.GetType().FullName + " does not support fd_write");
            }

            rights |= FileDescriptorRights.Write;
        }
    }

    /// <summary>Closes this file descriptor.</summary>
    /// <remarks>This provides the implementation for <c>fd_close</c>.</remarks>
    public void Close() {
        if (file != null) {
            file = null;
        }
    }

    void IDisposable.Dispose() => Close();
}
