namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

using System;

/// Provides implementations of WASI functions that return an error.
public sealed class Stubs {
    private const int ERROR = (int)Errno.InsufficientCapabilities;

    /// <summary>Provides an implementation of the <c>args_get</c> function.</summary>
    public static Func<int, int, int> ArgsGet { get; } = (int a, int b) => ERROR;

    /// <summary>Provides an implementation of the <c>args_sizes_get</c> function.</summary>
    public static Func<int, int, int> ArgsSizesGet { get; } = (int a, int b) => ERROR;

    /// <summary>Provides an implementation of the <c>environ_get</c> function.</summary>
    public static Func<int, int, int> EnvironGet { get; } = (int a, int b) => ERROR;

    /// <summary>Provides an implementation of the <c>environ_sizes_get</c> function.</summary>
    public static Func<int, int, int> EnvironSizesGet { get; } = (int a, int b) => ERROR;

    /// <summary>Provides an implementation of the <c>clock_res_get</c> function.</summary>
    public static Func<int, int, int> ClockResGet { get; } = (int a, int b) => ERROR;

    /// <summary>Provides an implementation of the <c>clock_time_get</c> function.</summary>
    public static Func<int, long, int, int> ClockTimeGet { get; } = (int a, long b, int c) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_advise</c> function.</summary>
    public static Func<int, long, long, int, int> FdAdvise { get; } = (int a, long b, long c, int d) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_allocate</c> function.</summary>
    public static Func<int, long, long, int> FdAllocate { get; } = (int a, long b, long c) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_close</c> function.</summary>
    public static Func<int, int> FdClose { get; } = (int fd) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_datasync</c> function.</summary>
    public static Func<int, int> FdDataSync { get; } = (int fd) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_fdstat_get</c> function.</summary>
    public static Func<int, int, int> FdStatGet { get; } = (int a, int b) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_fdstat_set_flags</c> function.</summary>
    public static Func<int, int, int> FdStatSetFlags { get; } = (int a, int b) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_fdstat_set_rights</c> function.</summary>
    public static Func<int, long, long, int> FdStatSetRights { get; } = (int a, long b, long c) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_filestat_get</c> function.</summary>
    public static Func<int, int, int> FdFileStatGet { get; } = (int a, int b) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_filestat_set_size</c> function.</summary>
    public static Func<int, long, int> FdFileStatSetSize { get; } = (int a, long b) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_filestat_set_times</c> function.</summary>
    public static Func<int, long, long, int, int> FdFileStatSetTimes { get; } = (int a, long b, long c, int d) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_pread</c> function.</summary>
    public static Func<int, int, int, long, int, int> FdPRead { get; } = (int a, int b, int c, long d, int e) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_prestat_get</c> function.</summary>
    public static Func<int, int, int> FdPreStatGet { get; } = (int a, int b) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_prestat_dir_name</c> function.</summary>
    public static Func<int, int, int, int> FdPreStatDirName { get; } = (int a, int b, int c) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_pwrite</c> function.</summary>
    public static Func<int, int, int, long, int, int> FdPWrite { get; } = (int a, int b, int c, long d, int e) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_read</c> function.</summary>
    public static Func<int, int, int, int, int> FdRead { get; } = (int a, int b, int c, int d) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_readdir</c> function.</summary>
    public static Func<int, int, int, long, int, int> FdReadDir { get; } = (int a, int b, int c, long d, int e) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_renumber</c> function.</summary>
    public static Func<int, int, int> FdRenumber { get; } = (int a, int b) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_seek</c> function.</summary>
    public static Func<int, long, int, int, int> FdSeek { get; } = (int a, long b, int c, int d) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_sync</c> function.</summary>
    public static Func<int, int> FdSync { get; } = (int fd) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_tell</c> function.</summary>
    public static Func<int, int, int> FdTell { get; } = (int a, int b) => ERROR;

    /// <summary>Provides an implementation of the <c>fd_write</c> function.</summary>
    public static Func<int, int, int, int, int> FdWrite { get; } = (int a, int b, int c, int d) => ERROR;

    /// <summary>Provides an implementation of the <c>path_create_directory</c> function.</summary>
    public static Func<int, int, int, int> PathCreateDirectory { get; } = (int a, int b, int c) => ERROR;

    /// <summary>Provides an implementation of the <c>path_filestat_get</c> function.</summary>
    public static Func<int, int, int, int, int, int> PathFileStatGet { get; } = (int a, int b, int c, int d, int e) => ERROR;

    /// <summary>Provides an implementation of the <c>path_filestat_set_times</c> function.</summary>
    public static Func<int, int, int, int, long, long, int, int> PathFileStatSetTimes { get; } = (int a, int b, int c, int d, long e, long f, int g) => ERROR;

    /// <summary>Provides an implementation of the <c>path_link</c> function.</summary>
    public static Func<int, int, int, int, int, int, int, int> PathLink { get; } = (int a, int b, int c, int d, int e, int f, int g) => ERROR;

    /// <summary>Provides an implementation of the <c>path_open</c> function.</summary>
    public static Func<int, int, int, int, int, long, long, int, int, int> PathOpen { get; } = (int a, int b, int c, int d, int e, long f, long g, int h, int i) => ERROR;

    /// <summary>Provides an implementation of the <c>path_readlink</c> function.</summary>
    public static Func<int, int, int, int, int, int, int> PathReadLink { get; } = (int a, int b, int c, int d, int e, int f) => ERROR;

    /// <summary>Provides an implementation of the <c>path_remove_directory</c> function.</summary>
    public static Func<int, int, int, int> PathRemoveDirectory { get; } = (int a, int b, int c) => ERROR;

    /// <summary>Provides an implementation of the <c>path_rename</c> function.</summary>
    public static Func<int, int, int, int, int, int, int> PathRename { get; } = (int a, int b, int c, int d, int e, int f) => ERROR;

    /// <summary>Provides an implementation of the <c>path_symlink</c> function.</summary>
    public static Func<int, int, int, int, int, int> PathSymLink { get; } = (int a, int b, int c, int d, int e) => ERROR;

    /// <summary>Provides an implementation of the <c>path_unlink_file</c> function.</summary>
    public static Func<int, int, int, int> PathUnlinkFile { get; } = (int a, int b, int c) => ERROR;

    /// <summary>Provides an implementation of the <c>poll_oneoff</c> function.</summary>
    public static Func<int, int, int, int, int> PollOneoff { get; } = (int a, int b, int c, int d) => ERROR;

    /// <summary>Provides an implementation of the <c>proc_raise</c> function.</summary>
    public static Func<int, int> ProcRaise { get; } = (int signal) => ERROR;

    /// <summary>Provides an implementation of the <c>sched_yield</c> function.</summary>
    public static Func<int> SchedYield { get; } = () => ERROR;

    /// <summary>Provides an implementation of the <c>random_get</c> function.</summary>
    public static Func<int, int, int> RandomGet { get; } = (int a, int b) => ERROR;

    /// <summary>Provides an implementation of the <c>random_get</c> function.</summary>
    public static Func<int, int, int, int> SockAccept { get; } = (int a, int b, int c) => ERROR;

    /// <summary>Provides an implementation of the <c>sock_recv</c> function.</summary>
    public static Func<int, int, int, int, int, int, int> SockRecv { get; } = (int a, int b, int c, int d, int e, int f) => ERROR;

    /// <summary>Provides an implementation of the <c>sock_send</c> function.</summary>
    public static Func<int, int, int, int, int, int> SockSend { get; } = (int a, int b, int c, int d, int e) => ERROR;

    /// <summary>Provides an implementation of the <c>sock_send</c> function.</summary>
    public static Func<int, int, int> SockShutdown { get; } = (int a, int b) => ERROR;
}
