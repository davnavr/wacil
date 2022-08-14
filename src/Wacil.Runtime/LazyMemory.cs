namespace Wacil.Runtime;

using System;

/// <summary>Provide a WebAssembly linear memory implementation that is lazily initialized.</summary>
/// <remarks>
/// This class allows providing WebAssembly function imports that need to manipulate a WebAssembly module's memory, especially in cases
/// where memory is not imported.
/// </remarks>
public sealed class LazyMemory<M> : IMemory32 where M : IMemory32 {
    private readonly Lazy<M> memory;

    /// <summary>Initializes a new instance of the <see cref="LazyMemory{M}"/> class.</summary>
    public LazyMemory(Lazy<M> memory) {
        this.memory = memory;
    }

    /// <inheritdoc/>
    public int PageCount { get => memory.Value.PageCount; }

    /// <inheritdoc/>
    public Limits Limits { get => memory.Value.Limits; }

    /// <inheritdoc/>
    public int Grow(int delta) => memory.Value.Grow(delta);

    /// <inheritdoc/>
    public byte this[int index] {
        get => memory.Value[index];
        set => memory.Value[index] = value;
    }

    /// <inheritdoc/>
    public short ReadInt16(int index, byte alignmentPowerHint) => memory.Value.ReadInt16(index, alignmentPowerHint);

    /// <inheritdoc/>
    public int ReadInt32(int index, byte alignmentPowerHint) => memory.Value.ReadInt32(index, alignmentPowerHint);

    /// <inheritdoc/>
    public long ReadInt64(int index, byte alignmentPowerHint) => memory.Value.ReadInt64(index, alignmentPowerHint);

    /// <inheritdoc/>
    public void Read(int index, Span<byte> buffer) => memory.Value.Read(index, buffer);

    /// <inheritdoc/>
    public void Write(int index, ReadOnlySpan<byte> bytes) => memory.Value.Write(index, bytes);

    /// <inheritdoc/>
    public void Fill(int index, int length, byte value) => memory.Value.Fill(index, length, value);

    /// <inheritdoc/>
    public void Write(int index, byte alignmentPowerHint, short value) => memory.Value.Write(index, alignmentPowerHint, value);

    /// <inheritdoc/>
    public void Write(int index, byte alignmentPowerHint, int value) => memory.Value.Write(index, alignmentPowerHint, value);

    /// <inheritdoc/>
    public void Write(int index, byte alignmentPowerHint, long value) => memory.Value.Write(index, alignmentPowerHint, value);

    /// <inheritdoc/>
    public void CopyTo<D>(int destinationIndex, int sourceIndex, int length, D destination) where D : IMemory32 {
        memory.Value.CopyTo<D>(destinationIndex, sourceIndex, length, destination);
    }

    /// <inheritdoc/>
    public void Dispose() {
        if (memory.IsValueCreated) {
            memory.Value.Dispose();
        }
    }
}
