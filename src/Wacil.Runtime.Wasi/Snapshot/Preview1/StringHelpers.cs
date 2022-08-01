namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

using System;
using System.Buffers;
using System.Text;
using Wacil.Runtime;

internal static class StringHelpers {
    internal static int WriteUtf8NullTerminated<T>(T memory, int index, string contents) where T : IMemory32 {
        int length = Encoding.UTF8.GetByteCount(contents) + 1;
        byte[]? rented = length <= 32 ? null : ArrayPool<byte>.Shared.Rent(length);
        Span<byte> buffer = rented == null ? stackalloc byte[length] : rented;

        Encoding.UTF8.GetBytes(contents, buffer);
        buffer[length] = 0; // Set null terminator
        memory.Write(index, buffer);

        if (rented != null) {
            ArrayPool<byte>.Shared.Return(rented);
        }

        return length;
    }
}
