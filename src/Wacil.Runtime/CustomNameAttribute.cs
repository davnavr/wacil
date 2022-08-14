namespace Wacil.Runtime;

using System;

/// <summary>Indicates that the specified member had an entry in the custom name section when it was translated from WebAssembly.</summary>
[AttributeUsage(AttributeTargets.Class | AttributeTargets.Delegate | AttributeTargets.Method | AttributeTargets.Property)]
public sealed class CustomNameAttribute : Attribute {
    /// <summary>The custom name.</summary>
    public string Name;

    /// <summary>Constructs a new instance of the <see cref="CustomNameAttribute"/> class.</summary>
    public CustomNameAttribute(string name) {
        Name = name;
    }
}
