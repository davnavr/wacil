namespace Wacil.Runtime;

using System;
using System.Runtime.CompilerServices;

/// <summary>Provides <see langword="static"/> methods for manipulating tables.</summary>
public static class TableHelpers {
    /// <summary>Obtains a <see langword="delegate"/> of a specific type from a function table.</summary>
    /// <remarks>
    /// If the obtained <see langword="delegate"/> does not match the expected type, a new delegate is created that calls the
    /// original delegate, replacing its location in the table.
    /// </remarks>
    public static F? GetFunction<F>(int index, Table<MulticastDelegate> table) where F : MulticastDelegate {
        MulticastDelegate? function = table[index];

        if (function is null) {
            return null;
        } else if (function is F instance) {
            return instance;
        } else {
            F converted = Unsafe.As<F>(Delegate.CreateDelegate(typeof(F), function.Target, function.Method));
            table[index] = converted;
            return converted;
        }
    }
}
