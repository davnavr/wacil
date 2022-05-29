namespace Wacil.Runtime {
    using System;
    using System.Buffers.Binary;
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

        //public static void Grow(Memory memory, uint pages)

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static int ReadInt32(Memory memory, uint offset, uint alignment, uint address) {
            var location = Location.FromAddress(unchecked(offset + address));
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
                return BinaryPrimitives.ReadInt32LittleEndian(memory.pages[location.Page]);
            } else {
                return memory.ReadInt32Slow(location);
            }
        }
    }
}
