namespace Wacil.Runtime;

using System;
using System.Buffers.Binary;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

/// <summary>Provides <see langword="static"/> methods for manipulating WebAssembly module memories.</summary>
public static class MemoryHelpers {
    /// The size, in bytes, of a WebAssembly page.
    public const int PageSize = 65536;

    /// Converts a number of bytes to a number of pages
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int ToPageCount(int bytes) => (int)((uint)bytes >> 16);

    /// Converts a number of pages into a number of bytes.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int ToByteSize(int pages) => pages << 16;

    /// Determines whether a given memory index is aligned on a 2 byte boundary.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool IsInt16Aligned(int index, byte alignmentPowerHint) {
        // Is the alignment hint at least 2 bytes and is the location a multiple of 2?
        return (index & 1) == 0 && alignmentPowerHint >= 1;
    }

    /// Determines whether a given memory index is aligned on a 4 byte boundary.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool IsInt32Aligned(int index, byte alignmentPowerHint) {
        // Is the alignment hint at least 4 bytes and is the location a multiple of 4?
        return (index & 0b11) == 0 && alignmentPowerHint >= 2;
    }

    /// Determines whether a given memory index is aligned on an 8 byte boundary.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static bool IsInt64Aligned(int index, byte alignmentPowerHint) {
        // Is the alignment hint at least 8 bytes and is the location a multiple of 8?
        return (index & 0b111) == 0 && alignmentPowerHint >= 3;
    }

    /// <summary>Provides the implementation for the <c>memory.grow</c> WebAssembly instruction.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int Grow<T>(int delta, T memory) where T : IMemory32 => memory.Grow(delta);

    /// <summary>Provides the implementation for the <c>memory.fill</c> WebAssembly instruction.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Fill<T>(int index, byte value, int length, T memory) where T : IMemory32 {
        memory.Fill(index, length, value);
    }

    /// Reads a byte at a specific location.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static byte ReadByte<T>(int index, T memory, int offset) where T : IMemory32 {
        return memory[index + offset];
    }

    /// Reads a 16-bit integer at a specific location.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static short ReadInt16<T>(int index, T memory, int offset, byte alignmentPowerHint) where T : IMemory32 {
        return memory.ReadInt16(index + offset, alignmentPowerHint);
    }

    /// Reads a 32-bit integer at a specific location.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static int ReadInt32<T>(int index, T memory, int offset, byte alignmentPowerHint) where T : IMemory32 {
        return memory.ReadInt32(index + offset, alignmentPowerHint);
    }

    /// Reads a 64-bit integer at a specific location.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static long ReadInt64<T>(int index, T memory, int offset, byte alignmentPowerHint) where T : IMemory32 {
        return memory.ReadInt64(index + offset, alignmentPowerHint);
    }

    /// Reads a 128-bit vector at a specific location.
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static Vector128 ReadVector128<T>(int index, T memory, int offset, byte alignmentPowerHint) where T : IMemory32 {
        return memory.ReadVector128(index + offset, alignmentPowerHint);
    }

    /// Reads a 16-bit integer at a specific location.
    public static short ReadInt16Slow<T>(T memory, int index) where T : IMemory32 {
        Span<byte> buffer = stackalloc byte[2];
        memory.Read(index, buffer);
        return BinaryPrimitives.ReadInt16LittleEndian(buffer);
    }

    /// Reads a 32-bit integer at a specific location.
    public static int ReadInt32Slow<T>(T memory, int index) where T : IMemory32 {
        Span<byte> buffer = stackalloc byte[4];
        memory.Read(index, buffer);
        return BinaryPrimitives.ReadInt32LittleEndian(buffer);
    }

    /// Reads a 64-bit integer at a specific location.
    public static long ReadInt64Slow<T>(T memory, int index) where T : IMemory32 {
        Span<byte> buffer = stackalloc byte[8];
        memory.Read(index, buffer);
        return BinaryPrimitives.ReadInt64LittleEndian(buffer);
    }

    /// Reads a 128-bit vector from a specific location.
    public static Vector128 ReadVector128Slow<T>(T memory, int index) where T : IMemory32 {
        Span<byte> buffer = stackalloc byte[Vector128.Size];
        memory.Read(index, buffer);
        return MemoryMarshal.Read<Vector128>(buffer);
    }

    /// <summary>Writes a byte <paramref name="value"/> to a specific location.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Write<T>(int index, byte value, T memory, int offset) where T : IMemory32 {
        memory[index + offset] = value;
    }

    /// <summary>Writes a 16-bit integer <paramref name="value"/> to a specific location.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Write<T>(int index, short value, T memory, int offset, byte alignmentPowerHint) where T : IMemory32 {
        memory.Write(index + offset, alignmentPowerHint, value);
    }

    /// <summary>Writes a 32-bit integer <paramref name="value"/> to a specific location.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Write<T>(int index, int value, T memory, int offset, byte alignmentPowerHint) where T : IMemory32 {
        memory.Write(index + offset, alignmentPowerHint, value);
    }

    /// <summary>Writes a 64-bit integer <paramref name="value"/> to a specific location.</summary>
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void Write<T>(int index, long value, T memory, int offset, byte alignmentPowerHint) where T : IMemory32 {
        memory.Write(index + offset, alignmentPowerHint, value);
    }

    /// <summary>Writes a 16-bit integer <paramref name="value"/> to a specific location.</summary>
    public static void WriteSlow<T>(T memory, int index, short value) where T : IMemory32 {
        Span<byte> buffer = stackalloc byte[2];
        BinaryPrimitives.WriteInt16LittleEndian(buffer, value);
        memory.Write(index, buffer);
    }

    /// <summary>Writes a 32-bit integer <paramref name="value"/> to a specific location.</summary>
    public static void WriteSlow<T>(T memory, int index, int value) where T : IMemory32 {
        Span<byte> buffer = stackalloc byte[4];
        BinaryPrimitives.WriteInt32LittleEndian(buffer, value);
        memory.Write(index, buffer);
    }

    /// <summary>Writes a 64-bit integer <paramref name="value"/> to a specific location.</summary>
    public static void WriteSlow<T>(T memory, int index, long value) where T : IMemory32 {
        Span<byte> buffer = stackalloc byte[8];
        BinaryPrimitives.WriteInt64LittleEndian(buffer, value);
        memory.Write(index, buffer);
    }

    /// <summary>
    /// Copies the specified <paramref name="bytes"/> into the specified <paramref name="memory"/> starting at the specified
    /// <paramref name="index"/>.
    /// </summary>
    public static void Write<T>(T memory, int index, byte[] bytes) where T : IMemory32 => memory.Write(index, bytes);

    internal static void CopySlow<S, D>(int destinationIndex, int sourceIndex, int length, S source, D destination)
        where S : IMemory32
        where D : IMemory32
    {
        for (int i = 0; i < length; i++) {
            source[sourceIndex + i] = destination[destinationIndex + i];
        }
    }

    /// <summary>Provides the implementation for the <c>memory.copy</c> WebAssembly instruction.</summary>
    public static void Copy<S, D>(int destinationIndex, int sourceIndex, int length, S source, D destination)
        where S : IMemory32
        where D : IMemory32
    {
        source.CopyTo<D>(destinationIndex, sourceIndex, length, destination);
    }
}
