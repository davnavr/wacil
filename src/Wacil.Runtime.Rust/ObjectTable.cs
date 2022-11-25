namespace Wacil.Runtime.Rust;

using System;
using System.Collections.Generic;

/// <summary>Maps integer indices to object references.</summary>
internal sealed class ObjectTable {
    internal const int NullIndex = -1;

    private readonly object locker = new();

    private readonly List<object?> lookup = new(capacity: 8);

    private int next = 0;

    internal ObjectTable() {}

    internal int Insert(object o) {
        lock (locker) {
            if (o == null) {
                return NullIndex;
            } else if (next < 0) {
                throw new InvalidOperationException("no indices remaining");
            }

            int index = next;

            if (next < lookup.Count) {
                lookup[next] = o;
            } else {
                lookup.Add(o);
            }

            next = lookup.Count;
            return index;
        }
    }

    internal object? Get(int index) {
        if (index == NullIndex) {
            return null;
        }

        lock (locker) {
            return lookup[index];
        }
    }

    /// <summary>
    /// Calls <see cref="object.GetHashCode"/> for the <see cref="object"/> corresponding to the given <paramref name="index"/>.
    /// </summary>
    /// <exception cref="ArgumentException">Thrown if the <paramref name="index"/> is the <see cref="NullIndex"/>.</exception>
    internal int GetHashCode(int index) {
        return (Get(index) ?? throw new ArgumentException(nameof(index))).GetHashCode();
    }

    internal void Remove(int index) {
        if (index == NullIndex) {
            throw new ArgumentException("attempt to free object corresponding to null index");
        }

        lock (locker) {
            if (lookup[index] == null) {
                throw new InvalidOperationException("double free detected for object #" + index);
            }

            lookup[index] = null;

            if (next > index) {
                next = index;
            }
        }
    }
}
