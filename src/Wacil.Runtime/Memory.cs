namespace Wacil.Runtime;

using System;
using System.Buffers.Binary;
using System.Collections.Generic;
using System.Runtime.CompilerServices;

/// Represents a WebAssembly linear memory.
[Obsolete]
public sealed class Memory {
    public const int PageSize = 65536;

    private readonly List<byte[]> pages; // TODO: Could have a readonly MemoryPage struct that eliminates bound checks in a page (A Uint16 index could be used)

    public Memory(int minimumPageCount = 0, int maximumPageCount = -1) {
        pages = new(minimumPageCount);

        for (int i = 0; i < minimumPageCount; i++) {
            pages.Add(new byte[PageSize]);
        }

        MinimumPageCount = minimumPageCount;
        MaximumPageCount = maximumPageCount;
    }

    public int PageCount => pages.Count;

    public int MinimumPageCount { get; init; }

    /// <summary>The maximum number of pages in this linear memory, or <c>-1</c> if there is no maximum number.</summary>
    public int MaximumPageCount { get; init; }

    private readonly record struct Location(int Page, ushort Offset) {
        /// The remaining number of bytes in the current page.
        internal int RemainingBytes => PageSize - Offset;

        internal static Location FromAddress(uint address) {
            unchecked {
                // Lower bits of address contain the actual offset from the start of the page
                return new((int)(address >> 16), (ushort)(address));
            }
        }

        public static Location operator + (Location location, ushort offset) {
            unchecked {
                uint nextOffset = (uint)offset + location.Offset;
                if (nextOffset < (uint)PageSize) {
                    return new(location.Page, (ushort)nextOffset);
                } else {
                    return new(location.Page, (ushort)(nextOffset - PageSize));
                }
            }
        }
    }
    
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static byte ReadByte(Memory memory, Location location) => memory.pages[location.Page][location.Offset];

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static byte ReadByte(Memory memory, uint address) => ReadByte(memory, Location.FromAddress(address));

    [MethodImpl(MethodImplOptions.NoInlining)]
    private int ReadInt32Slow(Location location) {
        Span<byte> buffer = stackalloc byte[4];
        buffer[0] = ReadByte(this, location);
        buffer[1] = ReadByte(this, location + 1);
        buffer[2] = ReadByte(this, location + 2);
        buffer[3] = ReadByte(this, location + 3);
        return BinaryPrimitives.ReadInt32LittleEndian(buffer);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private short ReadInt16Slow(Location location) {
        Span<byte> buffer = stackalloc byte[2];
        buffer[0] = ReadByte(this, location);
        buffer[1] = ReadByte(this, location + 1);
        return BinaryPrimitives.ReadInt16LittleEndian(buffer);
    }

    /// <summary>
    /// Attempts to increase the number of pages in the given <paramref name="memory"/> instance by the specified amount.
    /// </summary>
    /// <remarks>This is the implementation for the <c>memory.grow</c> instruction.</remarks>
    /// <returns>
    /// The previous length, in number of pages, of the memory instance, or <c>-1</c> if the memory could not be resized.
    /// </returns>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int Grow(int delta, Memory memory) {
        if (delta >= 0) {
            int previousCount = memory.pages.Count;
            memory.pages.Capacity += 4;
            while (delta > 0) {
                memory.pages.Add(new byte[PageSize]);
                delta--;
            }
            return previousCount;
        } else {
            return -1;
        }
    }

    // TODO: Maybe make alignment be in bytes instead of power of 2?
    /// <summary>
    /// Reads a 32-bit integer from the specified <paramref name="memory"/> at the specified <paramref name="address"/>.
    /// </summary>
    /// <remarks>This is the implementation for the <c>i32.load</c> instruction.</remarks>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int ReadInt32(uint address, Memory memory, uint offset, byte alignmentPower) {
        var location = Location.FromAddress(unchecked(offset + address));
        // Is the alignment hint >= 4 bytes and is the location a multiple of 4?
        if (alignmentPower >= 2 && (location.Offset & 0b11) == 0) {
            // An aligned read can occur
            // TODO: Could eleminate fixed boilerplate by using unmanaged buffers.
            //unsafe {
            //    fixed(byte* page = memory.pages[location.Page]) {
            //        return Unsafe.Read<int>((void*)(page + location.Offset));
            //    }
            //}
            // Temporary slower implementation
            return BinaryPrimitives.ReadInt32LittleEndian(
                new ReadOnlySpan<byte>(memory.pages[location.Page], location.Offset, 4)
            );
        } else {
            return memory.ReadInt32Slow(location);
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static short ReadInt16(uint address, Memory memory, uint offset, byte alignmentPower) {
        var location = Location.FromAddress(unchecked(offset + address));
        // Is the alignment hint >= 2 bytes and is the location a multiple of 2?
        if (alignmentPower >= 1 && (location.Offset & 1) == 0) {
            // An aligned read can occur
            // TODO: Could eleminate fixed boilerplate by using unmanaged buffers.
            //unsafe {
            //    fixed(byte* page = memory.pages[location.Page]) {
            //        return Unsafe.Read<int>((void*)(page + location.Offset));
            //    }
            //}
            // Temporary slower implementation
            return BinaryPrimitives.ReadInt16LittleEndian(
                new ReadOnlySpan<byte>(memory.pages[location.Page], location.Offset, 2)
            );
        } else {
            return memory.ReadInt16Slow(location);
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void WriteByte(Memory memory, Location location, byte value) =>
        memory.pages[location.Page][location.Offset] = value;

    [MethodImpl(MethodImplOptions.NoInlining)]
    private void WriteInt32Slow(Location location, int value) {
        Span<byte> buffer = stackalloc byte[4];
        BinaryPrimitives.WriteInt32LittleEndian(buffer, value);
        WriteByte(this, location, buffer[0]);
        WriteByte(this, location + 1, buffer[1]);
        WriteByte(this, location + 2, buffer[2]);
        WriteByte(this, location + 3, buffer[3]);
    }

    /// <summary>
    /// Writes a 32-bit integer to the specified <paramref name="memory"/> at the specified <paramref name="address"/>.
    /// </summary>
    /// <remarks>This is the implementation for the <c>i32.store</c> instruction.</remarks>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void WriteInt32(uint address, int value, Memory memory, uint offset, byte alignmentPower) {
        var location = Location.FromAddress(unchecked(offset + address));
        if (alignmentPower >= 2 && (location.Offset & 0b11) != 0) {
            // Temporary slower implementation
            BinaryPrimitives.WriteInt32LittleEndian(new Span<byte>(memory.pages[location.Page], location.Offset, 4), value);
        } else {
            memory.WriteInt32Slow(location, value);
        }
    }

    /// <summary>
    /// Writes the specified <paramref name="bytes"/> at the specified <paramref name="address"/>.
    /// </summary>
    public void Write(uint address, Span<byte> bytes) {
        var start = Location.FromAddress(address);
        var end = Location.FromAddress(address + (uint)bytes.Length);
        var remaining = bytes;
        for (int i = start.Page; i <= end.Page; i++) {
            var length = remaining.Length > PageSize ? PageSize : remaining.Length;

            if (i == start.Page && length > start.RemainingBytes) {
                length = start.RemainingBytes;
            }

            remaining.Slice(0, length).CopyTo(new Span<byte>(pages[i], i == start.Page ? start.Offset : 0, length));
            remaining = remaining.Slice(length);
        }
    }

    public void Read(uint address, Span<byte> destination) {
        // TODO: Avoid code duplication with Write
        var start = Location.FromAddress(address);
        var end = Location.FromAddress(address + (uint)destination.Length);
        var remaining = destination;
        for (int i = start.Page; i <= end.Page; i++) {
            var length = remaining.Length > PageSize ? PageSize : remaining.Length;

            if (i == start.Page && length > start.RemainingBytes) {
                length = start.RemainingBytes;
            }

            new Span<byte>(pages[i], i == start.Page ? start.Offset : 0, length).CopyTo(remaining);
            remaining = remaining.Slice(length);
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void Write(uint address, byte[] bytes) {
        Write(address, new Span<byte>(bytes));
    }
}
