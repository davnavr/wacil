namespace Wacil.Runtime.Rust;

using System;
using System.Collections.Generic;

/// <summary>Provides runtime support for Rust programs compiled with <c>wacil-bindgen</c>.</summary>
public sealed class RuntimeHelpers {
    private readonly ObjectTable objects = new();

    private readonly IReadOnlyDictionary<string, object> runtimeHelperImports;

    /// <summary>Initializes a new <see cref="RuntimeHelpers"/> instance.</summary>
    public RuntimeHelpers() {
        // Runtime functions, keep in sync with wacil-bindgen/src/runtime.rs
        runtimeHelperImports = new Dictionary<string, object>() {
            { "wacil_rt_object_table_drop", new Action<int>(objects.Remove) },
            { "wacil_rt_object_get_hash_code", new Func<int, int>(objects.GetHashCode) },
        };
    }


}
