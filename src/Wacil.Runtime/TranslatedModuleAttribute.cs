namespace Wacil.Runtime;

using System;

/// <summary>Attribute used to indicate that a generated class corresponds to a WebAssembly module.</summary>
[AttributeUsage(AttributeTargets.Constructor)]
public sealed class ModuleClassAttribute : Attribute {
    /// <summary>Constructs a <see cref="ModuleClassAttribute"/> instance.</summary>
    public ModuleClassAttribute() { }
}
