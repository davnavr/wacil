namespace Wacil.Runtime;

using System;
using System.Collections.Generic;

/// <summary>
/// Represents a WebAssembly linear memory backed by multiple allocations corresponding to each page.
/// </summary>
public sealed class SegmentedMemory : IMemory32 {
    private unsafe struct PageContents {
        internal fixed byte contents[MemoryHelpers.PageSize];
    }

    private sealed class Page {
        internal PageContents page;
    }

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

    private readonly List<Page> pages;

    /// <inheritdoc/>
    public MemoryType Limits { get; init; }

    public SegmentedMemory(MemoryType limits) {
        Limits = limits;
        pages = new List<Page>(limits.Minimum);

        for (int i = 0; i < limits.Minimum; i++) {
            pages.Add(new Page());
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
                    pages.Add(new Page());
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
                return pages[location.Page].page.contents[location.Offset];
            }
        }

        set {
            var location = Location.FromIndex(index);
            unsafe {
                pages[location.Page].page.contents[location.Offset] = value;
            }
        }
    }

    /// <inheritdoc/>
    public void Dispose() { }
}
