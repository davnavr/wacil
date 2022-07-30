namespace Wacil.Runtime;

using System;

/// Represents a WebAssembly memory type, which indicates the minimum and/or maximum size of a memory in pages.
public readonly struct MemoryType : IEquatable<MemoryType> {
    private readonly int minimum;
    private readonly int maximum;

    /// <summary>Constructs a new <see cref="MemoryType"/> with the specified minimum and/or maximum number of pages.</summary>
    public MemoryType(int minimum = 0, int maximum = -1) {
        if (minimum < 0) {
            throw new ArgumentOutOfRangeException(nameof(minimum), minimum, "minimum memory size cannot be negative");
        }

        if (maximum < -1) {
            throw new ArgumentOutOfRangeException(nameof(maximum));
        }

        if (minimum > maximum) {
            throw new ArgumentOutOfRangeException(nameof(minimum), minimum, "minimum memory size cannot be greater than maximum size");
        }

        this.minimum = minimum;
        this.maximum = maximum;
    }

    public int Minimum => minimum;

    public int Maximum => maximum;

    /// Gets the maximum number of allowed pages.
    public int Count => maximum == -1 ? int.MaxValue : maximum - minimum;

    public bool Equals(MemoryType other) => maximum == other.maximum && minimum == other.minimum;

    public override bool Equals(object? obj) {
        if (obj is MemoryType other) {
            return Equals(other);
        }

        return false;
    }
    
    public override int GetHashCode() => unchecked(maximum * minimum);

    public static bool operator == (MemoryType x, MemoryType y) => x.Equals(y);

    public static bool operator != (MemoryType x, MemoryType y) => !x.Equals(y);
}
