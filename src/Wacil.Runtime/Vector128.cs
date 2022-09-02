namespace Wacil.Runtime;

using System;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Runtime.Intrinsics;
using Intrinsics = System.Runtime.Intrinsics.Vector128;

// TODO: Will endianness be an issue? (.NET rarely runs on BE machines)

/// <summary>Provides an implementation for the <c>v128</c> WebAssembly type.</summary>
[StructLayout(LayoutKind.Explicit, Size = 16, Pack = 8)]
public readonly struct Vector128 : IEquatable<Vector128> {
    [FieldOffset(0)]
    private readonly Vector128<byte> bytes;

    [FieldOffset(0)]
    private readonly Vector128<short> shorts;

    [FieldOffset(0)]
    private readonly Vector128<int> integers;

    [FieldOffset(0)]
    private readonly Vector128<float> singles;

    [FieldOffset(0)]
    private readonly Vector128<long> longs;

    [FieldOffset(0)]
    private readonly Vector128<double> doubles;

    /// <summary>Initializes a <see cref="Vector128"/> from a vector of bytes.</summary>
    public Vector128(Vector128<byte> bytes) {
        Unsafe.SkipInit(out shorts);
        Unsafe.SkipInit(out integers);
        Unsafe.SkipInit(out singles);
        Unsafe.SkipInit(out longs);
        Unsafe.SkipInit(out doubles);
        this.bytes = bytes;
    }

    /// <summary>Initializes a <see cref="Vector128"/> from a vector of 16-bit integers.</summary>
    public Vector128(Vector128<short> shorts) {
        Unsafe.SkipInit(out bytes);
        Unsafe.SkipInit(out integers);
        Unsafe.SkipInit(out singles);
        Unsafe.SkipInit(out longs);
        Unsafe.SkipInit(out doubles);
        this.shorts = shorts;
    }

    /// <summary>Initializes a <see cref="Vector128"/> from a vector of 32-bit integers.</summary>
    public Vector128(Vector128<int> integers) {
        Unsafe.SkipInit(out bytes);
        Unsafe.SkipInit(out shorts);
        Unsafe.SkipInit(out singles);
        Unsafe.SkipInit(out longs);
        Unsafe.SkipInit(out doubles);
        this.integers = integers;
    }

    /// <summary>Initializes a <see cref="Vector128"/> from a vector of 32-bit floating point numbers.</summary>
    public Vector128(Vector128<float> singles) {
        Unsafe.SkipInit(out bytes);
        Unsafe.SkipInit(out shorts);
        Unsafe.SkipInit(out integers);
        Unsafe.SkipInit(out longs);
        Unsafe.SkipInit(out doubles);
        this.singles = singles;
    }

    /// <summary>Initializes a <see cref="Vector128"/> from a vector of 64-bit integers.</summary>
    public Vector128(Vector128<long> longs) {
        Unsafe.SkipInit(out bytes);
        Unsafe.SkipInit(out shorts);
        Unsafe.SkipInit(out integers);
        Unsafe.SkipInit(out singles);
        Unsafe.SkipInit(out doubles);
        this.longs = longs;
    }

    /// <summary>Initializes a <see cref="Vector128"/> from a vector of 64-bit floating point numbers.</summary>
    public Vector128(Vector128<double> doubles) {
        Unsafe.SkipInit(out bytes);
        Unsafe.SkipInit(out shorts);
        Unsafe.SkipInit(out integers);
        Unsafe.SkipInit(out singles);
        Unsafe.SkipInit(out longs);
        this.doubles = doubles;
    }

    /// <summary>Initializes a <see cref="Vector128"/> with eight <see cref="short"/> elements.</summary>
    public Vector128(short a, short b, short c, short d, short e, short f, short g, short h) : this(Intrinsics.Create(a, b, c, d, e, f, g, h)) {}

    /// <summary>Initializes a <see cref="Vector128"/> with four <see cref="int"/> elements.</summary>
    public Vector128(int a, int b, int c, int d) : this(Intrinsics.Create(a, b, c, d)) {}

    /// <summary>Initializes a <see cref="Vector128"/> with four <see cref="float"/> elements.</summary>
    public Vector128(float a, float b, float c, float d) : this(Intrinsics.Create(a, b, c, d)) {}

    /// <summary>Initializes a <see cref="Vector128"/> with two <see cref="long"/> elements.</summary>
    public Vector128(long a, long b) : this(Intrinsics.Create(a, b)) {}

    /// <summary>Initializes a <see cref="Vector128"/> with two <see cref="double"/> elements.</summary>
    public Vector128(double a, double b) : this(Intrinsics.Create(a, b)) {}

    /// <summary>Gets a vector with no bits set.</summary>
    public static Vector128 Zero { get; } = new(Vector128<byte>.Zero);

    /// <summary>Gets a vector with all bits set.</summary>
    public static Vector128 AllBitsSet { get; } = new(Vector128<byte>.AllBitsSet);

    /// <summary>Interprets the elements of this vector as a <see cref="Vector128{T}"/> of sixteen <see cref="byte"/> elements.</summary>
    public Vector128<byte> AsByte() => bytes;

    /// <summary>Interprets the elements of this vector as a <see cref="Vector128{T}"/> of eight <see cref="short"/> elements.</summary>
    public Vector128<short> AsInt16() => shorts;

    /// <summary>Interprets the elements of this vector as a <see cref="Vector128{T}"/> of four <see cref="int"/> elements.</summary>
    public Vector128<int> AsInt32() => integers;

    /// <summary>Interprets the elements of this vector as a <see cref="Vector128{T}"/> of four <see cref="float"/> elements.</summary>
    public Vector128<float> AsSingle() => singles;

    /// <summary>Interprets the elements of this vector as a <see cref="Vector128{T}"/> of two <see cref="long"/> elements.</summary>
    public Vector128<long> AsInt64() => longs;

    /// <summary>Interprets the elements of this vector as a <see cref="Vector128{T}"/> of two <see cref="double"/> elements.</summary>
    public Vector128<double> AsDouble() => doubles;

    /// <summary>Gets the <see cref="byte"/> element at the specified <paramref name="index"/>.</summary>
    public byte GetByte(int index) => bytes.GetElement(index);

    /// <summary>Gets the <see cref="short"/> element at the specified <paramref name="index"/>.</summary>
    public short GetInt16(int index) => shorts.GetElement(index);

    /// <summary>Gets the <see cref="int"/> element at the specified <paramref name="index"/>.</summary>
    public int GetInt32(int index) => integers.GetElement(index);

    /// <summary>Gets the <see cref="float"/> element at the specified <paramref name="index"/>.</summary>
    public float GetSingle(int index) => singles.GetElement(index);

    /// <summary>Gets the <see cref="long"/> element at the specified <paramref name="index"/>.</summary>
    public float GetInt64(int index) => longs.GetElement(index);

    /// <summary>Gets the <see cref="double"/> element at the specified <paramref name="index"/>.</summary>
    public double GetDouble(int index) => doubles.GetElement(index);

    /// <summary>Determines whether two vectors contain the same bits.</summary>
    public bool Equals(Vector128 other) => bytes.Equals(other.bytes);
}
