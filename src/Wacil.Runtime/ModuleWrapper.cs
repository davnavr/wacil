namespace Wacil.Runtime;

using System;
using System.Threading;

/// <summary>Base class for classes that serve as wrappers over translated WebAssembly modules.</summary>
public abstract class ModuleWrapper<T> where T : class {
    private readonly object locker = new();

    private readonly T module;

    /// <summary>Initializes an instance of the <see cref="ModuleWrapper{T}"/> class with the given <paramref name="module"/>.</summary>
    protected ModuleWrapper(T module) {
        ArgumentNullException.ThrowIfNull(module);
        this.module = module;
    }

    protected T Acquire() {
        Monitor.Enter(locker);
        return module;
    }

    protected void Release() {
        Monitor.Exit(locker);
    }
}
