namespace Wacil.Runtime;

using System;

/// Indicates that the specified member had a different name when it was translated from WebAssembly.
[AttributeUsage(AttributeTargets.Class | AttributeTargets.Delegate | AttributeTargets.Method | AttributeTargets.Property)]
public sealed class MangledNameAttribute : Attribute {
    /// The original name of the member as it was in the WebAssembly module.
    public string OriginalName;

    /// <summary>Constructs a new instance of the <see cref="MangledNameAttribute"/> class with the original name.</summary>
    public MangledNameAttribute(string original) {
        OriginalName = original;
    }
}
