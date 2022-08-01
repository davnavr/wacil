namespace Wacil.Runtime;

using System;

/// <summary>Represents a WebAssembly linear memory backed by a single <see langword="byte"/> array.</summary>
public sealed class ArrayMemory : IMemory32 {
    private byte[] buffer;

    /// <inheritdoc/>
    public Limits Limits { get; init; }

    /// <summary>Initializes a new linear memory with the specified <paramref name="limits"/>.</summary>
    public ArrayMemory(Limits limits) {
        Limits = limits;
        buffer = new byte[MemoryHelpers.ToByteSize(limits.Minimum)];
    }

    /// <inheritdoc/>
    public int PageCount => MemoryHelpers.ToPageCount(buffer.Length);

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

        Array.Resize<byte>(ref buffer, MemoryHelpers.ToByteSize(newPageCount));
        return oldPageCount;
    }

    /// <inheritdoc/>
    public void Write(int index, ReadOnlySpan<byte> bytes) => bytes.CopyTo(new Span<byte>(buffer, index, bytes.Length));

    /// <inheritdoc/>
    public void Read(int index, Span<byte> buffer) => new Span<byte>(this.buffer, index, buffer.Length).CopyTo(buffer);

    /// <inheritdoc/>
    public void Fill(int index, int length, byte value) => new Span<byte>(buffer, index, length).Fill(value);

    /// <inheritdoc/>
    public void CopyTo<D>(int destinationIndex, int sourceIndex, int length, D destination) where D : IMemory32 {
        destination.Write(destinationIndex, new ReadOnlySpan<byte>(buffer, sourceIndex, length));
    }

    void IDisposable.Dispose() { }
}
