namespace Wacil.Runtime;

using System;
using System.Reflection;

/// Constructs WASM module imports dynamically.
public static class DynamicImport {
    private static ConstructorInfo? GetImportConstructor<T>() {
        foreach (var constructor in typeof(T).GetConstructors(BindingFlags.Public)) {
            if (constructor.GetCustomAttribute<ImportConstructorAttribute>() != null) {
                return constructor;
            }
        }

        return null;
    }

    /// <summary>
    /// Attempts to create a new instance of the <typeparamref name="T"/> class, providing the specified arguments.
    /// </summary>
    /// <exception cref="InvalidOperationException">
    /// Thrown if the <typeparamref name="T"/> class does not contain a <see langword="public"/> constructor annotated with
    /// the <see cref="ImportConstructorAttribute"/> attribute.
    /// </exception>
    public static T Create<T>(IDynamicImportResolver imports) where T : class {
        ConstructorInfo? constructor = GetImportConstructor<T>();

        if (constructor == null) {
            throw new InvalidOperationException("No suitable constructor available for " + typeof(T).FullName);
        }

        ParameterInfo[] parameters = constructor.GetParameters();
        var arguments = new object?[parameters.Length];

        for (int i = 0; i < arguments.Length; i++) {
            arguments[i] = imports.Resolve(parameters[i].Name ?? String.Empty);
        }

        return (T)constructor.Invoke(arguments);
    }
}
