namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

/// <summary>Used by <see cref="ProcessExit.Throw"/> to indicate that a process has exited.</summary>
public sealed class ProcessExitException : System.Exception {
    /// The exit code returned by the process.
    public int ExitCode { get; init; }

    /// <summary>Constructs a <see cref="ProcessExitException"/> with a particular exit code.</summary>
    public ProcessExitException(int exitCode) : base("WASI process exited with code " + exitCode) {
        ExitCode = exitCode;
    }

    /// <summary>Throws a <see cref="ProcessExitException"/> with the specified exit code.</summary>
    public static void Throw(int exitCode) => throw new ProcessExitException(exitCode);
}
