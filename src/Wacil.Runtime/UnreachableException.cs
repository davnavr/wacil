namespace Wacil.Runtime;

using System;

/// <summary>
/// The exception that is thrown when a point is reached in translated code that corresponds to a WebAssembly <c>unreachable</c>
/// instruction.
/// </summary>
public sealed class UnreachableException : Exception {
    /// <summary>Constructs a new instance of the <see cref="UnreachableException"/> class.</summary>
    public UnreachableException() : base("Encountered unreachable point in code") {}
}
