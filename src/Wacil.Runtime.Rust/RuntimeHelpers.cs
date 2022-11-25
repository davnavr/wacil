namespace Wacil.Runtime.Rust;

using System;

/// <summary>Provides runtime support for Rust programs compiled with <c>wacil-bindgen</c>.</summary>
public sealed class RuntimeHelpers {
    private readonly ObjectTable objects = new();

    private sealed class ImportResolver : IDynamicImportResolver {
        private readonly IDynamicImportResolver imports;

        private readonly Action<int> rtObjectTableDrop;

        private readonly Func<int, int> rtObjectGetHashCode;

        internal ImportResolver(ObjectTable objects, IDynamicImportResolver imports) {
            this.imports = imports;
            this.rtObjectTableDrop = objects.Remove;
            this.rtObjectGetHashCode = objects.GetHashCode;
        }

        public object? Resolve(string name) {
            // Runtime functions, keep in sync with wacil-bindgen/src/runtime.rs
            // TODO: Replace wrappers to System.Object methods with auto-generated ones
            return name switch {
                "wacil_rt_object_table_drop" => rtObjectTableDrop,
                "wacil_rt_object_get_hash_code" => rtObjectGetHashCode,
                _ => imports.Resolve(name),
            };
        }
    }

    /// <summary>Initializes a new <see cref="RuntimeHelpers"/> instance.</summary>
    public RuntimeHelpers() {}

    /// <summary>Initializes an instance of the <typeparamref name="T"/> class.</summary>
    public T InitializeImports<T>(IDynamicImportResolver imports) where T : class {
        return Wacil.Runtime.DynamicImport.Create<T>(new ImportResolver(objects, imports));
    }
}
