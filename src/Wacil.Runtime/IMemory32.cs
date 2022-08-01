namespace Wacil.Runtime;

using System;
using System.Buffers.Binary;

/// <summary>Represents a WebAssembly linear memory with 32-bit addresses.</summary>
/// <remarks>
/// Some read and write methods provide a <c>alignmentPowerHint</c> argument, which implementors can use as a hint to
/// determine whether an index is considered aligned or not. If the index is not actually aligned, implementors should perform
/// an unaligned read/write instead.
/// </remarks>
/// <seealso cref="MemoryHelpers.PageSize"/>
public interface IMemory32 : IDisposable {
    /// The number of pages in this linear memory.
    public int PageCount { get; }

    /// Gets the minimum and maximum number of pages allowed by this linear memory.
    public Limits Limits { get; }

    /// <summary>Attempts to increase the number of pages in this linear memory by the specified amount.</summary>
    /// <remarks>This provides the implementation for the <c>memory.grow</c> instruction.</remarks>
    /// <returns>
    /// The previous length, in number of pages, of the memory instance, or <c>-1</c> if the memory could not be resized.
    /// </returns>
    public int Grow(int delta);

    /// <summary>Gets or sets a byte at the specified <paramref name="index"/>.</summary>
    /// <exception cref="IndexOutOfRangeException">Thrown when the <paramref name="index"/> is out of bounds.</exception>
    public byte this[int index] { get; set; }

    /// <summary>Reads a 16-bit integer at the specified <paramref name="index"/>.</summary>
    public short ReadInt16(int index, byte alignmentPowerHint) => MemoryHelpers.ReadInt16Slow<IMemory32>(this, index);

    /// <summary>Reads a 32-bit integer at the specified <paramref name="index"/>.</summary>
    /// <remarks>This provides the implementation for the <c>i32.load</c> instruction.</remarks>
    public int ReadInt32(int index, byte alignmentPowerHint) => MemoryHelpers.ReadInt32Slow<IMemory32>(this, index);

    /// <summary>Reads a 64-bit integer at the specified <paramref name="index"/>.</summary>
    /// <remarks>This provides the implementation for the <c>i64.load</c> instruction.</remarks>
    public long ReadInt64(int index, byte alignmentPowerHint) => MemoryHelpers.ReadInt64Slow<IMemory32>(this, index);

    /// <summary>
    /// Copies bytes starting at the specified <paramref name="index"/> into the specified <paramref name="buffer"/>.
    /// </summary>
    public void Read(int index, Span<byte> buffer) {
        for (int i = 0; i < buffer.Length; i++) {
            buffer[i] = this[index + i];
        }
    }

    /// <summary>
    /// Writes the specified <paramref name="bytes"/> to the this memory starting at the specified <paramref name="index"/>.
    /// </summary>
    public void Write(int index, ReadOnlySpan<byte> bytes) {
        for (int offset = 0; offset < bytes.Length; offset++) {
            this[index + offset] = bytes[offset];
        }
    }

    /// <summary>
    /// Fills a region of memory of the specified <paramref name="length"/> starting at the specified <paramref name="index"/>
    /// with the specified <paramref name="value"/>.
    /// </summary>
    public void Fill(int index, int length, byte value) {
        var end = index + length;
        for (int i = index; i < end; i++) {
            this[i] = value;
        }
    }

    /// <summary>
    /// Writes a 16-bit integer at the specified <paramref name="index"/>.
    /// </summary>
    public void Write(int index, byte alignmentPowerHint, short value) => MemoryHelpers.WriteSlow<IMemory32>(this, index, value);

    /// <summary>
    /// Writes a 32-bit integer at the specified <paramref name="index"/>.
    /// </summary>
    /// <remarks>This provides the implementation for the <c>i64.store</c> instruction.</remarks>
    public void Write(int index, byte alignmentPowerHint, int value) => MemoryHelpers.WriteSlow<IMemory32>(this, index, value);

    /// <summary>
    /// Writes a 64-bit integer at the specified <paramref name="index"/>.
    /// </summary>
    /// <remarks>This provides the implementation for the <c>i64.store</c> instruction.</remarks>
    public void Write(int index, byte alignmentPowerHint, long value) => MemoryHelpers.WriteSlow<IMemory32>(this, index, value);
}
