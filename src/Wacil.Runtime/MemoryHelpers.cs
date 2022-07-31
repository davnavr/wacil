namespace Wacil.Runtime;

using System;
using System.Buffers.Binary;
using System.Runtime.CompilerServices;

/// <summary>Provides <see langword="static"/> methods for manipulating WebAssembly module memories.</summary>
public static class MemoryHelpers {
    /// The size, in bytes, of a WebAssembly page.
    public const int PageSize = 65536;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int ToPageCount(int bytes) => (int)((uint)bytes >> 16);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int ToByteSize(int pages) => pages << 16;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool IsInt16Aligned(int index, byte alignmentPowerHint) {
        // Is the alignment hint at least 2 bytes and is the location a multiple of 2?
        return (index & 1) == 0 && alignmentPowerHint >= 1;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool IsInt32Aligned(int index, byte alignmentPowerHint) {
        // Is the alignment hint at least 4 bytes and is the location a multiple of 4?
        return (index & 0b11) == 0 && alignmentPowerHint >= 2;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool IsInt64Aligned(int index, byte alignmentPowerHint) {
        // Is the alignment hint at least 8 bytes and is the location a multiple of 8?
        return (index & 0b111) == 0 && alignmentPowerHint >= 3;
    }

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

    public static short ReadInt16Slow<T>(T memory, int index) where T : IMemory32 {
        Span<byte> buffer = stackalloc byte[2];
        memory.Read(index, buffer);
        return BinaryPrimitives.ReadInt16LittleEndian(buffer);
    }

    public static int ReadInt32Slow<T>(T memory, int index) where T : IMemory32 {
        Span<byte> buffer = stackalloc byte[4];
        memory.Read(index, buffer);
        return BinaryPrimitives.ReadInt32LittleEndian(buffer);
    }

    public static long ReadInt64Slow<T>(T memory, int index) where T : IMemory32 {
        Span<byte> buffer = stackalloc byte[8];
        memory.Read(index, buffer);
        return BinaryPrimitives.ReadInt64LittleEndian(buffer);
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

    public static void WriteSlow<T>(T memory, int index, short value) where T : IMemory32 {
        Span<byte> buffer = stackalloc byte[2];
        BinaryPrimitives.WriteInt16LittleEndian(buffer, value);
        memory.Write(index, buffer);
    }

    public static void WriteSlow<T>(T memory, int index, int value) where T : IMemory32 {
        Span<byte> buffer = stackalloc byte[4];
        BinaryPrimitives.WriteInt32LittleEndian(buffer, value);
        memory.Write(index, buffer);
    }

    public static void WriteSlow<T>(T memory, int index, long value) where T : IMemory32 {
        Span<byte> buffer = stackalloc byte[8];
        BinaryPrimitives.WriteInt64LittleEndian(buffer, value);
        memory.Write(index, buffer);
    }

    public static void Write<T>(T memory, int index, byte[] bytes) where T : IMemory32 => memory.Write(index, bytes);
}
