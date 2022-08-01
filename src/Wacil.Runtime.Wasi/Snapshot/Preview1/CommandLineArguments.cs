namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

using System;
using System.Collections.Generic;
using Wacil.Runtime;

/// <summary>Represents command line arguments.</summary>
/// <remarks>
/// This class is meant to provide implementations for the <c>args_get</c> and <c>args_sizes_get</c> functions.
/// </remarks>
public sealed class CommandLineArguments {
    private readonly IMemory32 memory;

    private readonly IReadOnlyCollection<string> arguments;

    /// <summary>Constructs a new instance of the <see cref="CommandLineArguments"/> class.</summary>
    public CommandLineArguments(IMemory32 memory, IReadOnlyCollection<string> arguments) {
        ArgumentNullException.ThrowIfNull(arguments);
        this.memory = memory;
        this.arguments = arguments;
    }

    /// Gets the underlying collection providing the command line arguments.
    public IReadOnlyCollection<string> Arguments => arguments;
}
