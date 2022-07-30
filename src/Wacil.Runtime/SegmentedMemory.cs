namespace Wacil.Runtime;

using System;
using System.Buffers.Binary;
using System.Collections.Generic;

/// <summary>
/// Represents a WebAssembly linear memory backed by multiple allocations corresponding to each page.
/// </summary>
public sealed class SegmentedMemory : IMemory32 {
    private readonly record struct Location(int Page, ushort Offset) {
        /// The remaining number of bytes in the current page.
        internal int RemainingBytes => MemoryHelpers.PageSize - Offset;

        internal static Location FromIndex(int index) {
            unchecked {
                // Lower bits of address contain the actual offset from the start of the page
                return new(index >> 16, (ushort)(index));
            }
        }
    }

    // TODO: Eliminate bounds checks w/ DangerousGetReferenceAt
    private readonly List<byte[]> pages;

    /// <inheritdoc/>
    public MemoryType Limits { get; init; }

    public SegmentedMemory(MemoryType limits) {
        Limits = limits;
        pages = new List<byte[]>(limits.Minimum);

        for (int i = 0; i < limits.Minimum; i++) {
            pages.Add(new byte[MemoryHelpers.PageSize]);
        }
    }

    /// <inheritdoc/>
    public int PageCount => pages.Count;

    /// <inheritdoc/>
    public int Grow(int delta) {
        if (delta >= 0) {
            int oldPageCount = pages.Count;

            if (delta != 0) {
                pages.EnsureCapacity(pages.Count + delta);

                while (delta > 0) {
                    pages.Add(new byte[MemoryHelpers.PageSize]);
                    delta--;
                }
            }

            return oldPageCount;
        } else {
            return -1;
        }
    }

    /// <inheritdoc/>
    public byte this[int index] {
        get {
            var location = Location.FromIndex(index);
            unsafe {
                return pages[location.Page][location.Offset];
            }
        }

        set {
            var location = Location.FromIndex(index);
            unsafe {
                pages[location.Page][location.Offset] = value;
            }
        }
    }

    /// <inheritdoc/>
    public short ReadInt16(int index, byte alignmentPowerHint) {
        if (MemoryHelpers.IsInt16Aligned(index, alignmentPowerHint)) {
            var location = Location.FromIndex(index);
            return BinaryPrimitives.ReadInt16LittleEndian(new ReadOnlySpan<byte>(pages[location.Page], location.Offset, 2));
        }

        return MemoryHelpers.ReadInt16Slow<SegmentedMemory>(this, index);
    }

    /// <inheritdoc/>
    public int ReadInt32(int index, byte alignmentPowerHint) {
        if (MemoryHelpers.IsInt32Aligned(index, alignmentPowerHint)) {
            var location = Location.FromIndex(index);
            return BinaryPrimitives.ReadInt32LittleEndian(new ReadOnlySpan<byte>(pages[location.Page], location.Offset, 4));
        }

        return MemoryHelpers.ReadInt32Slow<SegmentedMemory>(this, index);
    }

    /// <inheritdoc/>
    public long ReadInt64(int index, byte alignmentPowerHint) {
        if (MemoryHelpers.IsInt64Aligned(index, alignmentPowerHint)) {
            var location = Location.FromIndex(index);
            return BinaryPrimitives.ReadInt64LittleEndian(new ReadOnlySpan<byte>(pages[location.Page], location.Offset, 8));
        }

        return MemoryHelpers.ReadInt64Slow<SegmentedMemory>(this, index);
    }

    /// <inheritdoc/>
    public void Write(int index, ReadOnlySpan<byte> bytes) {
        if (bytes.IsEmpty) {
            return;
        }

        var start = Location.FromIndex(index);
        var end = Location.FromIndex(index + bytes.Length);
        var remaining = bytes;
        for (int i = start.Page; i <= end.Page; i++) {
            var length = remaining.Length > MemoryHelpers.PageSize ? MemoryHelpers.PageSize : remaining.Length;

            if (i == start.Page && length > start.RemainingBytes) {
                length = start.RemainingBytes;
            }

            remaining.Slice(0, length).CopyTo(new Span<byte>(pages[i], i == start.Page ? start.Offset : 0, length));
            remaining = remaining.Slice(length);
        }
    }

    /// <inheritdoc/>
    public void Write(int index, byte alignmentPowerHint, short value) {
        if (MemoryHelpers.IsInt16Aligned(index, alignmentPowerHint)) {
            var location = Location.FromIndex(index);
            BinaryPrimitives.WriteInt16LittleEndian(new Span<byte>(pages[location.Page], location.Offset, 2), value);
        } else {
            MemoryHelpers.WriteSlow<SegmentedMemory>(this, index, value);
        }
    }

    /// <inheritdoc/>
    public void Write(int index, byte alignmentPowerHint, int value) {
        if (MemoryHelpers.IsInt32Aligned(index, alignmentPowerHint)) {
            var location = Location.FromIndex(index);
            BinaryPrimitives.WriteInt32LittleEndian(new Span<byte>(pages[location.Page], location.Offset, 4), value);
        } else {
            MemoryHelpers.WriteSlow<SegmentedMemory>(this, index, value);
        }
    }
    /// <inheritdoc/>
    public void Write(int index, byte alignmentPowerHint, long value) {
        if (MemoryHelpers.IsInt64Aligned(index, alignmentPowerHint)) {
            var location = Location.FromIndex(index);
            BinaryPrimitives.WriteInt64LittleEndian(new Span<byte>(pages[location.Page], location.Offset, 8), value);
        } else {
            MemoryHelpers.WriteSlow<SegmentedMemory>(this, index, value);
        }
    }

    /// <inheritdoc/>
    public void Dispose() { }
}
