namespace Wacil.Runtime;

using System.Runtime.CompilerServices;

/// <summary>Provides <see langword="static"/> methods for manipulating WebAssembly module memories.</summary>
public static class MemoryHelpers {
    /// The size, in bytes, of a WebAssembly page.
    public const int PageSize = 65536;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static int ToPageCount(int bytes) => (int)((uint)bytes >> 16);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    internal static int ToByteSize(int pages) => pages << 16;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int Grow<T>(int delta, T memory) where T : IMemory32 => memory.Grow(delta);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static byte ReadByte<T>(int index, T memory, int offset) where T : IMemory32 {
        return memory[index + offset];
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static short ReadInt16<T>(int index, T memory, int offset, byte alignmentPowerHint) where T : IMemory32 {
        return memory.ReadInt16(index + offset, alignmentPowerHint);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int ReadInt32<T>(int index, T memory, int offset, byte alignmentPowerHint) where T : IMemory32 {
        return memory.ReadInt32(index + offset, alignmentPowerHint);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long ReadInt64<T>(int index, T memory, int offset, byte alignmentPowerHint) where T : IMemory32 {
        return memory.ReadInt64(index + offset, alignmentPowerHint);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Write<T>(int index, byte value, T memory, int offset) where T : IMemory32 {
        memory[index + offset] = value;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Write<T>(int index, short value, T memory, int offset, byte alignmentPowerHint) where T : IMemory32 {
        memory.Write(index + offset, alignmentPowerHint, value);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Write<T>(int index, int value, T memory, int offset, byte alignmentPowerHint) where T : IMemory32 {
        memory.Write(index + offset, alignmentPowerHint, value);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Write<T>(int index, long value, T memory, int offset, byte alignmentPowerHint) where T : IMemory32 {
        memory.Write(index + offset, alignmentPowerHint, value);
    }

    public static void Write<T>(T memory, int index, byte[] bytes) where T : IMemory32 => memory.Write(index, bytes);
}
