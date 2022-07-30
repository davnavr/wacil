namespace Wacil.Runtime;

using System;

/// Represents WebAssembly limits, which indicate the minimum and/or maximum size of a memory in pages or the number of elements
/// in a table.
public readonly struct Limits : IEquatable<Limits> {
    private readonly int minimum;
    private readonly int maximum;

    /// <summary>Constructs a new <see cref="Limits"/> with the specified minimum and/or maximum.</summary>
    public Limits(int minimum = 0, int maximum = -1) {
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

    /// Gets the maximum number of allowed memory pages or table elements.
    public int Count => maximum == -1 ? int.MaxValue : maximum - minimum;

    public bool Contains(int value) => value >= minimum && (maximum == -1 || value <= maximum);

    public bool Equals(Limits other) => maximum == other.maximum && minimum == other.minimum;

    public override bool Equals(object? obj) {
        if (obj is Limits other) {
            return Equals(other);
        }

        return false;
    }
    
    public override int GetHashCode() => unchecked(maximum * minimum);

    public static bool operator == (Limits x, Limits y) => x.Equals(y);

    public static bool operator != (Limits x, Limits y) => !x.Equals(y);
}
