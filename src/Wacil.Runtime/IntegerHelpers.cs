namespace Wacil.Runtime;

using System.Numerics;
using System.Runtime.CompilerServices;

public static class IntegerHelpers {
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int RotateLeft(int amount, int value) => (int)BitOperations.RotateLeft((uint)value, amount);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int RotateRight(int amount, int value) => (int)BitOperations.RotateRight((uint)value, amount);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long RotateLeft(long amount, long value) => (long)BitOperations.RotateLeft((ulong)value, (int)amount);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long RotateRight(long amount, long value) => (long)BitOperations.RotateRight((ulong)value, (int)amount);
}
