namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

using System;

/// Provides implementations of WASI functions that return an error.
public sealed class Stubs {
    private const int ERROR = (int)Errno.InsufficientCapabilities;

    /// <summary>Provides an implementation of the <c>args_get</c> function.</summary>
    public static Func<int, int, int> ArgsGet { get; } = (int a, int b) => ERROR;

    /// <summary>Provides an implementation of the <c>args_sizes_get</c> function.</summary>
    public static Func<int, int, int> ArgsSizesGet { get; } = (int a, int b) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_write</c> function.</summary>
    public static Func<int, int, int, int, int> FdWrite { get; } = (int a, int b, int c, int d) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_close</c> function.</summary>
    public static Func<int, int> FdClose { get; } = (int fd) => ERROR;
}
