namespace Wacil.Runtime {
    using System;
    using System.Collections.Generic;
    using System.Runtime.CompilerServices;

    /// Represents a WebAssembly linear memory.
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

        public int MinimumPageCount { get; init; }

        /// <summary>The maximum number of pages in this linear memory, or <c>-1</c> if there is no maximum number.</summary>
        public int MaximumPageCount { get; init; }

        private readonly record struct Location(int Page, ushort Offset) {
            /// The remaining number of bytes in the current page.
            internal int RemainingBytes => PageSize - Offset;

            internal static Location FromAddress(uint address) {
                // Lower bits of address contain the actual offset from the start of the page
                return new((int)(address >> 16), unchecked((ushort)(address)));
            }
        }

        [MethodImpl(MethodImplOptions.NoInlining)]
        private int ReadInt32Slow(Location location) {

        }

        //public static void Grow(Memory memory, uint pages)

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static int ReadInt32(Memory memory, uint offset, uint alignment, uint address) {
            var location = Location.FromAddress(offset + address);
            // Is the alignment hint >= 4 bytes and is the location a multiple of 4?
            if (alignment >= 2 && (location.Offset & 0b11) != 0) {
                // An aligned read can occur
                // TODO: Could eleminate fixed boilerplate by using unmanaged buffers.
                //unsafe {
                //    fixed(byte* page = memory.pages[location.Page]) {
                //        return Unsafe.Read<int>((void*)(page + location.Offset));
                //    }
                //}
                // Temporary slower implementation
                return BitConverter.ToInt32(memory.pages[location.Page]);
            } else {
                return memory.ReadInt32Slow(location);
            }
        }
    }
}
