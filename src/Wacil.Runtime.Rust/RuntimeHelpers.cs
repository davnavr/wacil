namespace Wacil.Runtime.Rust;

/// <summary>Provides runtime support for Rust programs compiled with <c>wacil-bindgen</c>.</summary>
public sealed class RuntimeHelpers {
    private readonly ObjectTable objects = new();

    /// <summary>Initializes a new <see cref="RuntimeHelpers"/> instance.</summary>
    public RuntimeHelpers() {}
}
