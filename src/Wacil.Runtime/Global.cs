namespace Wacil.Runtime;

/// <summary>
/// <p>Represents a global variable accesible from both .NET and translated WebAssembly code.</p>
/// <p>This class is not thread safe.</p>
/// </summary>
public sealed class Global<T> {
    private T value;

    private readonly bool mutable;

    /// <summary>Instantiates a new global variable with the specified <paramref name="initial"/> value and mutability.</summary>
    /// <param name="initial">The initial value stored in the global variable.</param>
    /// <param name="mutable">If set to <see langword="true"/>, allows setting the value of the global variable.</param>
    public Global(T initial, bool mutable = false) {
        value = initial;
        this.mutable = mutable;
    }

    /// <summary>Gets a value indicating whether the value of this global variable can be changed.</summary>
    public bool Mutable => mutable;

    /// <summary>Gets or sets the value of the global variable.</summary>
    /// <exception cref="T:System.InvalidOperationException">
    /// Thrown if an attempt was made to modify an immutable global variable.
    /// </exception>
    public T Value {
        get => value;

        set {
            if (!mutable) {
                throw new System.InvalidOperationException("Attempt to mutate an immutable global variable");
            }

            this.value = value;
        }
    }

    public override string ToString() => this.value?.ToString() ?? "null";
}
