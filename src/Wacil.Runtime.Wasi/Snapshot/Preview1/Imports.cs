namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

using System;
using Wacil.Runtime;

/// Provides delegates that are used to provide WASI functions to translated WASM modules.
public static class Imports {
    /// <summary>Provides the <c>args_get</c> function.</summary>
    public static Func<int, int, int> ArgsGet<M>(CommandLineArguments<M> args) where M : IMemory32 => args.GetArguments;

    /// <summary>Provides the <c>args_sizes_get</c> function.</summary>
    public static Func<int, int, int> ArgsSizesGet<M>(CommandLineArguments<M> args) where M : IMemory32 => args.GetArgumentSizes;

    /// <summary>Provides the <c>fd_write</c> function.</summary>
    public static Func<int, int, int, int, int> FdWrite<M>(FileSystem<M> fs) where M : IMemory32 => fs.Write;
}
