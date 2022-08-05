namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

using System;
using System.IO;

/// <summary>Helps log calls to WASI functions.</summary>
public sealed class Logger {
    private readonly TextWriter output;

    /// <summary>
    /// Initializes a <see cref="Logger"/> instance with the specified <see cref="TextWriter"/> that messages are written to.
    /// </summary>
    public Logger(TextWriter output) {
        this.output = output;
    }

    /// <summary>
    /// Initializes a <see cref="Logger"/> instance that writes messages to the standard output stream.
    /// </summary>
    public Logger() : this(System.Console.Out) { }

    private void WriteResult(int? result) {
        output.Write(" -> ");
        output.WriteLine(result == null ? "!" : (Errno)result);
    }

    /// <summary>Wraps an implementation of the <c>args_get</c> function.</summary>
    public Func<int, int, int> ArgsGet(Func<int, int, int> getter) => (int argv, int buffer) => {
        int? result = null;
        try {
            result = getter(argv, buffer);
            return result.Value;
        } finally {
            output.Write("args_get(argv = (byte**)0x{0:X}, argv_buf = (byte*)0x{1:X})", argv, buffer);
            WriteResult(result);
        }
    };

    /// <summary>Wraps an implementation of the <c>args_sizes_get</c> function.</summary>
    public Func<int, int, int> ArgsSizesGet(Func<int, int, int> getter) => (int count, int size) => {
        int? result = null;
        try {
            result = getter(count, size);
            return result.Value;
        } finally {
            output.Write("args_sizes_get(count = (byte*)0x{0:X}, size = (byte*)0x{1:X})", count, size);
            WriteResult(result);
        }
    };

    /// <summary>Wraps an implementation of the <c>environ_get</c> function.</summary>
    public Func<int, int, int> EnvironGet(Func<int, int, int> getter) => (int environ, int buffer) => {
        int? result = null;
        try {
            result = getter(environ, buffer);
            return result.Value;
        } finally {
            output.Write("environ_get(environ = (byte**)0x{0:X}), argv_buf = (byte*)0x{1:X})", environ, buffer);
        }
    };

    /// <summary>Wraps an implementation of the <c>environ_sizes_get</c> function.</summary>
    public Func<int, int, int> EnvironSizesGet(Func<int, int, int> getter) => (int count, int size) => {
        int? result = null;
        try {
            result = getter(count, size);
            return result.Value;
        } finally {
            output.Write("environ_sizes_get(count = (byte*)0x{0:X}, size = (byte*)0x{1:X})", count, size);
            WriteResult(result);
        }
    };

    /// <summary>Wraps an implementation of the <c>proc_exit</c> function.</summary>
    public Action<int> ProcExit(Action<int> exiter) => (int code) => {
        output.Write("proc_exit(code = {0})", code);
        exiter(code);
    };

    /// <summary>Wraps an implementation of the <c>fd_write</c> function.</summary>
    public Func<int, int, int, int, int> FdWrite(Func<int, int, int, int, int> writer) {
        return (int fd, int ioVectorPointer, int ioVectorCount, int bytesWrittenPointer) => {
            int? result = null;
            try {
                result = writer(fd, ioVectorPointer, ioVectorCount, bytesWrittenPointer);
                return result.Value;
            } finally {
                output.Write("fd_write(fd = {0}, iovs = (ciovec*)0x{1:X}), iovs_len = {2}, nwritten = (int*)0x{3:X})", fd, ioVectorPointer, ioVectorCount, bytesWrittenPointer);
                WriteResult(result);
            }
        };
    }

    /// <summary>Wraps an implementation of the <c>fd_close</c> function.</summary>
    public Func<int, int> FdClose(Func<int, int> implementation) => (int fd) => {
        int? result = null;
        try {
            result = implementation(fd);
            return result.Value;
        } finally {
            output.Write("fd_close(fd = {0})", fd);
            WriteResult(result);
        }
    };
}
