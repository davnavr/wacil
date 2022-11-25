namespace Wacil.Runtime;

using System.Collections.Generic;

/// <summary>Provides an import resolver from an <see cref="IReadOnlyDictionary{TKey, TValue}"/>.</summary>
public sealed class DictionaryImportResolver : IDynamicImportResolver {
    private readonly IReadOnlyDictionary<string, object> imports;

    /// <summary>Initializes a <see cref="DictionaryImportResolver"/> with a dictionary.</summary>
    public DictionaryImportResolver(IReadOnlyDictionary<string, object> imports) {
        this.imports = imports;
    }

    /// <inheritdoc />
    public object? Resolve(string name) {
        object? value = null;
        imports.TryGetValue(name, out value);
        return value;
    }
}
