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

    /// <summary>Wraps an implementation of the <c>args_get</c> function.</summary>
    public Func<int, int, int> ArgsGet(Func<int, int, int> getter) => (int argv, int buffer) => {
        output.Write("args_get(argv = (byte**)0x{0:X}, argv_buf = (byte*)0x{1:X})", argv, buffer);
        int result = getter(argv, buffer);
        output.WriteLine(" -> {0}", (Errno)result);
        return result;
    };

    /// <summary>Wraps an implementation of the <c>args_sizes_get</c> function.</summary>
    public Func<int, int, int> ArgsSizesGet(Func<int, int, int> getter) => (int count, int size) => {
        output.Write("args_sizes_get(count = (byte*)0x{0:X}, size = (byte*)0x{1:X})", count, size);
        int result = getter(count, size);
        output.WriteLine(" -> {0}", (Errno)result);
        return result;
    };

    /// <summary>Wraps an implementation of the <c>proc_exit</c> function.</summary>
    public Action<int> ProcExit(Action<int> exiter) => (int code) => {
        output.Write("proc_exit(code = {0})", code);
        exiter(code);
    };

    /// <summary>Wraps an implementation of the <c>fd_write</c> function.</summary>
    public Func<int, int, int, int, int> FdWrite(Func<int, int, int, int, int> writer) {
        return (int fd, int ioVectorPointer, int ioVectorCount, int bytesWrittenPointer) => {
            output.Write("fd_write(fd = {0}, iovs = (ciovec*)0x{1:X}), iovs_len = {2}, ", fd, ioVectorPointer, ioVectorCount);
            output.Write("nwritten = (int*)0x{0:X})", bytesWrittenPointer);
            int result = writer(fd, ioVectorPointer, ioVectorCount, bytesWrittenPointer);
            output.WriteLine(" -> {0}", (Errno)result);
            return result;
        };
    }

    /// <summary>Wraps an implementation of the <c>fd_close</c> function.</summary>
    public Func<int, int> FdClose(Func<int, int> implementation) => (int fd) => {
        output.Write("fd_close(fd = {0})", fd);
        int result = implementation(fd);
        output.WriteLine(" -> {0}", (Errno)result);
        return result;
    };
}
