namespace Wacil.Runtime;

using System.Numerics;
using System.Runtime.CompilerServices;

/// <summary>Provides <see langword="static"/> methods for manipulating integer values.</summary>
public static class IntegerHelpers {
    /// <summary>Rotates a 32-bit integer left by the specified <paramref name="amount"/>.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int RotateLeft(int amount, int value) => (int)BitOperations.RotateLeft((uint)value, amount);

    /// <summary>Rotates a 32-bit integer right by the specified <paramref name="amount"/>.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int RotateRight(int amount, int value) => (int)BitOperations.RotateRight((uint)value, amount);

    /// <summary>Rotates a 64-bit integer left by the specified <paramref name="amount"/>.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long RotateLeft(long amount, long value) => (long)BitOperations.RotateLeft((ulong)value, (int)amount);

    /// <summary>Rotates a 64-bit integer right by the specified <paramref name="amount"/>.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long RotateRight(long amount, long value) => (long)BitOperations.RotateRight((ulong)value, (int)amount);
}
