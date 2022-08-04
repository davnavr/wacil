namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Text;
using Wacil.Runtime;

/// <summary>Represents environment variables.</summary>
/// <remarks>
/// This class is meant to provide implementations for the <c>args_get</c> and <c>args_sizes_get</c> functions.
/// </remarks>
public sealed class EnvironmentVariables<M> where M : IMemory32 {
    private const byte SEPARATOR = (byte)'=';

    private readonly M memory;

    private sealed class StringComparer : IEqualityComparer<ImmutableArray<byte>> {
        public bool Equals(ImmutableArray<byte> x, ImmutableArray<byte> y) {
            if (x.Length != y.Length) {
                return false;
            }

            for(int index = 0; index < x.Length; index++) {
                if (x[index] != y[index]) {
                    return false;
                }
            }

            return true;
        }

        public int GetHashCode(ImmutableArray<byte> data) {
            var hash = new HashCode();
            hash.AddBytes(data.AsSpan());
            return hash.ToHashCode();
        }

        public static StringComparer Instance { get; } = new();
    }

    private readonly Dictionary<ImmutableArray<byte>, ImmutableArray<byte>> variables = new(StringComparer.Instance);

    private int totalByteSize = 0;

    /// <summary>Initializes an empty <see cref="EnvironmentVariables{M}"/> instance.</summary>
    public EnvironmentVariables(M memory) {
        ArgumentNullException.ThrowIfNull(memory);
        this.memory = memory;
    }

    private static void ValidateBytes(string name, ImmutableArray<byte> bytes) {
        foreach (byte value in bytes) {
            if (value == SEPARATOR) {
                throw new ArgumentOutOfRangeException(name, "string should not contain a separator (the '=' character)");
            }
        }
    }

    /// <summary>Attempts to define an environment variable.</summary>
    /// <exception cref="ArgumentOutOfRangeException">
    /// Thrown when the <paramref name="name"/> or <paramref name="value"/> contains a <c>NUL</c> byte or a separator
    /// (<c>=</c> character) byte.
    /// </exception>
    /// <returns>
    /// <see langword="true"/> if the environment variable was successfully defined, or <see langword="false"/> if a value is already
    /// associated with the variable.
    /// </returns>
    public bool TryDefine(ImmutableArray<byte> name, ImmutableArray<byte> value) {
        ValidateBytes(nameof(name), name);
        ValidateBytes(nameof(value), value);
        return variables.TryAdd(name, value);
    }
}
