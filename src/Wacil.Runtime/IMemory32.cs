namespace Wacil.Runtime;

using System;
using System.Buffers.Binary;

/// <summary>Represents a WebAssembly linear memory with 32-bit addresses.</summary>
/// <remarks>
/// Some read and write methods provide a <c>expectedAlignmentPower</c> argument, which implementors can use as a hint to
/// determine whether an index is considered aligned or not. If the index is not actually aligned, implementors should perform
/// an unaligned read/write instead.
/// </remarks>
/// <seealso cref="MemoryHelpers.PageSize"/>
public interface IMemory32 : IDisposable {
    /// The number of pages in this linear memory.
    public int PageCount { get; }

    /// Gets the minimum and maximum number of pages allowed by this linear memory.
    public MemoryType Limits { get; }

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
    public short ReadInt16(int index, byte expectedAlignmentPower) {
        Span<byte> buffer = stackalloc byte[2];
        buffer[0] = this[index];
        buffer[1] = this[index + 1];
        return BinaryPrimitives.ReadInt16LittleEndian(buffer);
    }

    /// <summary>Reads a 32-bit integer at the specified <paramref name="index"/>.</summary>
    /// <remarks>This provides the implementation for the <c>i32.load</c> instruction.</remarks>
    public int ReadInt32(int index, byte expectedAlignmentPower) {
        Span<byte> buffer = stackalloc byte[4];
        buffer[0] = this[index];
        buffer[1] = this[index + 1];
        buffer[2] = this[index + 2];
        buffer[3] = this[index + 3];
        return BinaryPrimitives.ReadInt32LittleEndian(buffer);
    }

    /// <summary>Reads a 64-bit integer at the specified <paramref name="index"/>.</summary>
    /// <remarks>This provides the implementation for the <c>i64.load</c> instruction.</remarks>
    public long ReadInt64(int index, byte expectedAlignmentPower) {
        Span<byte> buffer = stackalloc byte[8];
        buffer[0] = this[index];
        buffer[1] = this[index + 1];
        buffer[2] = this[index + 2];
        buffer[3] = this[index + 3];
        buffer[4] = this[index + 4];
        buffer[5] = this[index + 5];
        buffer[6] = this[index + 6];
        buffer[7] = this[index + 7];
        return BinaryPrimitives.ReadInt64LittleEndian(buffer);
    }

    /// <summary>
    /// Writes the specified <paramref name="bytes"/> to the this memory starting at the specified <paramref name="index"/>.
    /// </summary>
    public void Write(int index, Span<byte> bytes) {
        for (int offset = 0; offset < bytes.Length; offset++) {
            this[index + offset] = bytes[offset];
        }
    }

    public void Fill(int index, int length, byte value) {
        var end = index + length;
        for (int i = index; i < end; i++) {
            this[i] = value;
        }
    }

    /// <summary>
    /// Writes a 16-bit integer to the specified <paramref name="memory"/> at the specified <paramref name="index"/>.
    /// </summary>
    public void Write(int index, byte byteExpectedAlignmentPower, short value) {
        Span<byte> buffer = stackalloc byte[2];
        BinaryPrimitives.WriteInt16LittleEndian(buffer, value);
        Write(index, buffer);
    }

    /// <summary>
    /// Writes a 32-bit integer to the specified <paramref name="memory"/> at the specified <paramref name="index"/>.
    /// </summary>
    /// <remarks>This provides the implementation for the <c>i64.store</c> instruction.</remarks>
    public void Write(int index, byte byteExpectedAlignmentPower, int value) {
        Span<byte> buffer = stackalloc byte[4];
        BinaryPrimitives.WriteInt32LittleEndian(buffer, value);
        Write(index, buffer);
    }

    /// <summary>
    /// Writes a 64-bit integer to the specified <paramref name="memory"/> at the specified <paramref name="index"/>.
    /// </summary>
    /// <remarks>This provides the implementation for the <c>i64.store</c> instruction.</remarks>
    public void Write(int index, byte byteExpectedAlignmentPower, long value) {
        Span<byte> buffer = stackalloc byte[8];
        BinaryPrimitives.WriteInt64LittleEndian(buffer, value);
        Write(index, buffer);
    }
}
