namespace Wacil.Runtime;

using System;

/// Represents WebAssembly limits, which indicate the minimum and/or maximum size of a memory in pages or the number of elements
/// in a table.
public readonly struct Limits : IEquatable<Limits> {
    private readonly int minimum;
    private readonly int maximum;

    /// <summary>Initializes <see cref="Limits"/> with the specified minimum and/or maximum.</summary>
    public Limits(int minimum = 0, int maximum = -1) {
        if (minimum < 0) {
            throw new ArgumentOutOfRangeException(nameof(minimum), minimum, "minimum memory size cannot be negative");
        }

        if (maximum < -1) {
            throw new ArgumentOutOfRangeException(nameof(maximum));
        }

        if (maximum != -1 && minimum > maximum) {
            throw new ArgumentOutOfRangeException(nameof(minimum), minimum, "minimum memory size cannot be greater than maximum size");
        }

        this.minimum = minimum;
        this.maximum = maximum;
    }

    /// The minimum number of memory pages or table elements.
    public int Minimum => minimum;

    /// The maximum allowed number of memory pages or table elements.
    public int Maximum => maximum;

    /// Determines whether the specified count is equal to
    public bool Contains(int value) => value >= minimum && (maximum == -1 || value <= maximum);

    /// <inheritdoc/>
    public bool Equals(Limits other) => maximum == other.maximum && minimum == other.minimum;

    /// <inheritdoc/>
    public override bool Equals(object? obj) {
        if (obj is Limits other) {
            return Equals(other);
        }

        return false;
    }

    /// <inheritdoc/>
    public override int GetHashCode() => unchecked(maximum * minimum);

    /// <inheritdoc/>
    public static bool operator == (Limits x, Limits y) => x.Equals(y);

    /// <inheritdoc/>
    public static bool operator != (Limits x, Limits y) => !x.Equals(y);
}
