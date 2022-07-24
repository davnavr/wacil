namespace Wacil.Runtime;

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

    public E? this[int index] {
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        get => Get(index, this);
        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        set => Set(index, value, this);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static E? Get(int index, Table<E> table) {
        return table.elements[index];
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Set(int index, E? item, Table<E> table) {
        table.elements[index] = item;
    }

    /// <summary>Copies elements in to the <paramref name="table"/>.</summary>
    /// <remarks>This method provides the implementation for the <c>table.init</c> WebAssembly instruction.</remarks>
    public static void Initialize(int count, int elementStartIndex, int tableStartIndex, Table<E> table, E?[] elements) {
        if (elementStartIndex + count > table.elements.Count || count > elements.Length) {
            throw new System.ArgumentOutOfRangeException(nameof(count));
        }

        for (int index = 0; index < count; index++) {
            table.elements[tableStartIndex + index] = elements[elementStartIndex + index];
        }
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
