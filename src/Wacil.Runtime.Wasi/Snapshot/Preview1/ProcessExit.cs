namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

using System;

/// <summary>Contains implementations of the WASI <c>proc_exit</c> function.</summary>
public static class ProcessExit {
    /// <summary>Terminates the process by throwing a <see cref="ProcessExitException"/>.</summary>
    public static Action<int> Throw { get; } = ProcessExitException.Throw;

    /// <summary>Terminates the process using <see cref="Environment.Exit(int)"/>.</summary>
    public static Action<int> Exit { get; } = Environment.Exit;
}
