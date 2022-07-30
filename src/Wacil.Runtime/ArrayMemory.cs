namespace Wacil.Runtime;

using System;

/// <summary>Represents a WebAssembly linear memory backed by a single <see langword="byte"/> array.</summary>
public sealed class ArrayMemory : IMemory32 {
    private byte[] buffer;

    /// <inheritdoc/>
    public MemoryType Limits { get; init; }

    public ArrayMemory(MemoryType limits) {
        Limits = limits;
        buffer = new byte[limits.Minimum];
    }

    /// <inheritdoc/>
    public int PageCount => buffer.Length / MemoryHelpers.PageSize;

    /// <inheritdoc/>
    public byte this[int index] {
        get => buffer[index];
        set => buffer[index] = value;
    }

    /// <inheritdoc/>
    public int Grow(int delta) {
        int oldPageCount = PageCount;
        int newPageCount = oldPageCount + delta;

        if (newPageCount < 0) {
            return -1;
        }

        Array.Resize<byte>(ref buffer, newPageCount * MemoryHelpers.PageSize);
        return oldPageCount;
    }

    void IDisposable.Dispose() { }
}
