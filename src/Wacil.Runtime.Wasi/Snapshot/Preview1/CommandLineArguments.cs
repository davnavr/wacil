namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Text;
using Wacil.Runtime;

/// <summary>Represents command line arguments.</summary>
/// <remarks>
/// This class is meant to provide implementations for the <c>args_get</c> and <c>args_sizes_get</c> functions.
/// </remarks>
public sealed class CommandLineArguments<T> where T : IMemory32 {
    private readonly T memory;

    private readonly List<ImmutableArray<byte>> arguments = new();

    private int totalArgumentSize = 0;

    /// <summary>Constructs a new instance of the <see cref="CommandLineArguments{T}"/> class.</summary>
    public CommandLineArguments(T memory) {
        ArgumentNullException.ThrowIfNull(memory);
        this.memory = memory;
    }

    /// Adds an argument.
    public void Add(ImmutableArray<byte> argument) {
        if (argument.Contains(0)) {
            throw new ArgumentException("Arguments must not contain any null bytes", nameof(argument));
        }

        totalArgumentSize += argument.Length + 1;
        arguments.Add(argument);
    }

    /// <summary>Adds an argument <see langword="string"/>, converting it to a UTF-8 null-terminated string.</summary>
    public void Add(string argument) => Add(ImmutableArray.Create<byte>(Encoding.UTF8.GetBytes(argument)));

    /// <summary>
    /// Writes the command line arguments into the specified <paramref name="buffer"/>, and the pointers to each individual
    /// argument into the <paramref name="argv"/> array.
    /// </summary>
    /// <remarks>This provides an implementation for the <c>args_get</c> function.</remarks>
    public int GetArguments(int argv, int buffer) {
        int argumentPointerIndex = argv;
        int argumentBufferIndex = buffer;
        for (int index = 0; index < arguments.Count; index++) {
            var arg = arguments[index].AsSpan();

            try {
                memory.Write(argumentPointerIndex, 2, argumentBufferIndex); // Write the pointer to the argument
                memory.Write(argumentBufferIndex, arg);
                memory[arg.Length] = 0; // Write the null terminator.
            } catch {
                return (int)Errno.Fault;
            }

            argumentPointerIndex += 4;
            argumentBufferIndex += arg.Length + 1;
        }

        return (int)Errno.Success;
    }

    /// <remarks>This provides an implementation for the <c>args_sizes_get</c> function.</remarks>
    public int GetArgumentSizes(int argumentCountPointer, int argumentSizePointer) {
        try {
            memory.Write(argumentCountPointer, 2, arguments.Count);
            memory.Write(argumentSizePointer, 2, totalArgumentSize);
            return (int)Errno.Success;
        } catch {
            return (int)Errno.Fault;
        }
    }
}
