namespace Wacil.Runtime {
    using System;
    using System.Collections.Generic;

    /// Represents a WebAssembly linear memory.
    public sealed class Memory {
        public const int PageSize = 65536;

        private readonly List<byte[]> pages; // TODO: Could have a readonly MemoryPage struct that eliminates bound checks in a page (A Uint16 index could be used)

        public Memory(int minimumPageCount = 0, int? maximumPageCount = null) {
            pages = new(minimumPageCount);

            for (int i = 0; i < minimumPageCount; i++) {
                pages.Add(new byte[PageSize]);
            }

            MinimumPageCount = minimumPageCount;
            MaximumPageCount = maximumPageCount;
        }

        public int MinimumPageCount { get; init; }

        public int? MaximumPageCount { get; init; }

        private readonly record struct Location(int page, ushort offset);

        private static Location GetPageIndex(uint address) {
            // Math.DivRem
            return new((int)(address / (uint)PageSize), (ushort)(address % (uint)PageSize));
        }
    }
}
