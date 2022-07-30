namespace Wacil.Runtime;

using System.Runtime.CompilerServices;

/// <summary>Provides <see langword="static"/> methods for manipulating WebAssembly module memories.</summary>
public static class MemoryHelpers<T> where T : IMemory32 {
    /// The size, in bytes, of a WebAssembly page.
    public const int PageSize = 65536;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int Grow(int delta, T memory) => memory.Grow(delta);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static byte ReadByte(int index, T memory, int offset) {
        return memory[index + offset];
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static short ReadInt16(int index, T memory, int offset, byte alignmentPowerHint) {
        return memory.ReadInt16(index + offset, alignmentPowerHint);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int ReadInt32(int index, T memory, int offset, byte alignmentPowerHint) {
        return memory.ReadInt32(index + offset, alignmentPowerHint);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long ReadInt64(int index, T memory, int offset, byte alignmentPowerHint) {
        return memory.ReadInt64(index + offset, alignmentPowerHint);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Write(int index, byte value, T memory, int offset) {
        memory[index + offset] = value;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Write(int index, short value, T memory, int offset, byte alignmentPowerHint) {
        memory.Write(index + offset, alignmentPowerHint, value);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Write(int index, int value, T memory, int offset, byte alignmentPowerHint) {
        memory.Write(index + offset, alignmentPowerHint, value);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Write(int index, long value, T memory, int offset, byte alignmentPowerHint) {
        memory.Write(index + offset, alignmentPowerHint, value);
    }

    public static void Write(T memory, int index, byte[] bytes) => memory.Write(index, bytes);
}
