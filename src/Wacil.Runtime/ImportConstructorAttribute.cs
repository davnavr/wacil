namespace Wacil.Runtime;

using System;

/// Attribute used to indicate that a generated constructor corresponds to a WebAssembly module import.
[AttributeUsage(AttributeTargets.Constructor)]
public sealed class ImportConstructorAttribute : Attribute {
    /// <inheritdoc/>
    public ImportConstructorAttribute() { }
}
