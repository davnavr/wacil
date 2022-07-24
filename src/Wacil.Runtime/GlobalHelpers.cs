namespace Wacil.Runtime;

using System.Runtime.CompilerServices;

/// <summary>Provides <see langword="static"/> methods for manipulating global variables.</summary>
public static class GlobalHelpers {
    /// <summary>Attempts to set the value for a global variable.</summary>
    /// <remarks>This method provides the implementation for the <c>global.set</c> WebAssembly instruction.</remarks>
    /// <exception cref="T:System.InvalidOperationException">
    /// Thrown if the <paramref name="variable"/> was not mutable.
    /// </exception>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void SetValue<T>(T value, Global<T> variable) => variable.Value = value;
}
