namespace Wacil.Runtime.Rust;

using System;
using System.Collections.Generic;

/// <summary>Maps integer indices to object references.</summary>
internal sealed class ObjectTable {
    private readonly object locker = new();

    private readonly List<object?> lookup = new(capacity: 8);

    private int next = 0;

    internal ObjectTable() {}

    internal int Insert(object o) {
        lock (locker) {
            if (o == null) {
                return -1;
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
        lock (locker) {
            return lookup[index];
        }
    }

    internal void Remove(int index) {
        lock (locker) {
            if (index >= lookup.Count) {
                throw new IndexOutOfRangeException("index too large");
            }

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
