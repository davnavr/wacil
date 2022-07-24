namespace Wacil.Runtime;

using System;

/// Indicates that the specified member had a different name when it was translated from WebAssembly.
[AttributeUsage(AttributeTargets.Class | AttributeTargets.Delegate | AttributeTargets.Method | AttributeTargets.Property)]
public sealed class MangledNameAttribute : Attribute {
    public string OriginalName;

    public MangledNameAttribute(string original) {
        OriginalName = original;
    }
}
