namespace Wacil.Runtime;

/// <summary>Provides imports for translated WASM modules.</summary>
/// <seealso cref="DynamicImport"/>
public interface IDynamicImportResolver {
    /// <summary>Attempts to resolve an import corresponding to the given <paramref name="name"/>.</summary>
    /// <returns>
    /// An <see langword="object"/> corresponding to the import, which can be a <see cref="Global{T}"/>, a <see cref="IMemory32"/>, a
    /// <see cref="Table{E}"/>, or a <see langword="delegate"/>; or <see langword="null"/> if the import could not be resolved.
    /// </returns>
    public object? Resolve(string name);
}
