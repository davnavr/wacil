namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

using System;
using System.Buffers;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.IO;
using Wacil.Runtime;

/// <summary>Provides a mapping between WASI file descriptors and <see cref="FileDescriptor"/> instances.</summary>
public sealed class FileSystem<M> where M : IMemory32 {
    private readonly byte[] smallByteBuffer = new byte[256];

    private readonly M memory;

    /// <summary>Gets a dictionary which maps integers to file descriptors.</summary>
    public IDictionary<int, FileDescriptor> Descriptors { get; init; }

    /// <summary>Initializes a <see cref="FileSystem{M}"/> instance.</summary>
    public FileSystem(M memory, IDictionary<int, FileDescriptor> descriptors) {
        this.memory = memory;
        Descriptors = descriptors;
    }

    /// <summary>Writes to a file descriptor.</summary>
    /// <remarks>This provides an implementation for the <c>fd_write</c> WASI function.</remarks>
    /// <returns>An <see cref="Errno"/> value indicating if the write was successful.</returns>
    public int Write(int fd, int ioVectorPointer, int ioVectorCount, int bytesWrittenPointer) {
        FileDescriptor? descriptor;
        if (!Descriptors.TryGetValue(fd, out descriptor)) {
            return (int)Errno.BadFileDescriptor;
        }

        if (!descriptor.CanWrite) {
            return (int)Errno.InsufficientCapabilities;
        }

        int bytesWrittenCount = 0;
        Span<byte> ciovec = stackalloc byte[8];

        while (ioVectorCount > 0) {
            bool rented;
            byte[] buffer;
            Span<byte> bytes;

            try {
                memory.Read(ioVectorPointer, ciovec);
                int ioBufferPointer = BinaryPrimitives.ReadInt32LittleEndian(ciovec.Slice(0, 4));
                int ioBufferLength = BinaryPrimitives.ReadInt32LittleEndian(ciovec.Slice(4, 4));
                ioVectorPointer += 8;

                rented = ioBufferLength > smallByteBuffer.Length;
                buffer = rented ? ArrayPool<byte>.Shared.Rent(ioBufferLength) : smallByteBuffer;
                bytes = new Span<byte>(buffer, 0, ioBufferLength); 
                memory.Read(ioBufferPointer, bytes);
            } catch {
                return (int)Errno.Fault;
            }

            if (rented) {
                ArrayPool<byte>.Shared.Return(buffer);
            }

            try {
                descriptor.File.Write(bytes);
            } catch (IOException) {
                return (int)Errno.IO;
            } catch (ObjectDisposedException) {
                return (int)Errno.BadFileDescriptor;
            }

            bytesWrittenCount += bytes.Length;
            ioVectorCount--;
        }

        try {
            memory.Write(bytesWrittenPointer, 2, bytesWrittenCount);
        } catch {
            return(int)Errno.Fault;
        }

        return (int)Errno.Success;
    }

    /// <summary>Closes a file descriptor.</summary>
    /// <returns>An <see cref="Errno"/> value indicating if the file descriptor was successfully closed.</returns>
    public int Close(int fd) {
        FileDescriptor? descriptor;
        if (!Descriptors.TryGetValue(fd, out descriptor) || descriptor.IsClosed) {
            return (int)Errno.BadFileDescriptor;
        }

        descriptor.Close();

        return (int)Errno.Success;
    }
}
