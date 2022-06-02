namespace Wacil.Runtime {
    using System;
    using System.Buffers.Binary;
    using System.Collections.Generic;
    using System.Runtime.CompilerServices;

    public sealed class Table<E> where E : class {
        private readonly List<E?> elements;

        public Table(int minimumSize = 0, int maximumSize = -1) {
            elements = new(minimumSize);

            for (int i = 0; i < minimumSize; i++) {
                elements.Add(null);
            }
        }

        public int Size {
            [MethodImpl(MethodImplOptions.AggressiveInlining)]
            get => elements.Count;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static E? Get(int index, Table<E> table) {
            return table.elements[index];
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static void Set(int index, E? item, Table<E> table) {
            table.elements[index] = item;
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        public static int Grow(E? initial, int delta, Table<E> table) {
            if (delta >= 0) {
                int previousSize = table.elements.Count;
                table.elements.EnsureCapacity(previousSize + delta);
                while (delta > 0) {
                    table.elements.Add(initial);
                    delta--;
                }
                return previousSize;
            } else {
                return -1;
            }
        }
    }
}