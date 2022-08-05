namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

/// <summary>Indicates the operations that are allowed for a particular file descriptor.</summary>
[System.Flags]
public enum FileDescriptorRights : long {
    /// <summary>No actions are allowed for this file descriptor.</summary>
    None = 0,
    /// <summary>Calling <c>fd_datasync</c> is allowed.</summary>
    DataSync = 1,
    /// <summary>Calling <c>fd_read</c> is allowed.</summary>
    Read = 2,
    /// <summary>Calling <c>fd_seek</c> is allowed.</summary>
    Seek = 4,
    /// <summary>Calling <c>fd_fdstat_set_flags</c> is allowed.</summary>
    SetStatFlags = 8,
    /// <summary>Calling <c>fd_sync</c> is allowed.</summary>
    Sync = 0x10,
    /// <summary>Calling <c>fd_tell</c> is allowed.</summary>
    Tell = 0x20,
    /// <summary>Calling <c>fd_write</c> is allowed.</summary>
    Write = 0x40,
    /// <summary>Calling <c>fd_advise</c> is allowed.</summary>
    Advise = 0x80,
    /// <summary>Calling <c>fd_allocate</c> is allowed.</summary>
    Allocate = 0x100,
    /// <summary>Calling <c>path_create_directory</c> is allowed.</summary>
    CreateDirectory = 0x200,
    /// <summary>Calling <c>path_create_file</c> is allowed.</summary>
    CreateFile = 0x400,
    /// <summary>Calling <c>path_link_source</c> is allowed.</summary>
    LinkSource = 0x800,
    /// <summary>Calling <c>path_link_target</c> is allowed.</summary>
    LinkTarget = 0x1000,
    /// <summary>Calling <c>path_open</c> is allowed.</summary>
    Open = 0x2000,
    /// <summary>Calling <c>fd_readdir</c> is allowed.</summary>
    ReadDirectory = 0x4000,
    /// <summary>Calling <c>path_readlink</c> is allowed.</summary>
    ReadLink = 0x8000,
    /// <summary>Calling <c>path_rename_source</c> is allowed.</summary>
    RenameSource = 0x10000,
    /// <summary>Calling <c>path_rename_target</c> is allowed.</summary>
    RenameTarget = 0x20000,
    /// <summary>Calling <c>path_filestat_get</c> is allowed.</summary>
    GetPathStat = 0x40000,
    /// <summary>Calling <c>path_filestat_set_size</c> is allowed.</summary>
    SetPathStatSize = 0x80000,
    /// <summary>Calling <c>path_filestat_set_times</c> is allowed.</summary>
    SetPathStatTimes = 0x100000,
    /// <summary>Calling <c>fd_filestat_get</c> is allowed.</summary>
    GetStat = 0x200000,
    /// <summary>Calling <c>fd_filestat_set_size</c> is allowed.</summary>
    SetStatSize = 0x400000,
    /// <summary>Calling <c>fd_filestat_set_times</c> is allowed.</summary>
    SetStatTimes = 0x800000,
    /// <summary>Calling <c>path_symlink</c> is allowed.</summary>
    CreateSymbolicLink = 0x1000000,
    /// <summary>Calling <c>path_remove_directory</c> is allowed.</summary>
    RemoveDirectory = 0x2000000,
    /// <summary>Calling <c>path_unlink_file</c> is allowed.</summary>
    UnlinkFile = 0x4000000,
    /// <summary>Calling <c>poll_fd_readwrite</c> is allowed.</summary>
    PollReadWrite = 0x8000000,
    /// <summary>Calling <c>sock_shutdown</c> is allowed.</summary>
    SockShutdown = 0x10000000,
    /// <summary>Calling <c>sock_accept</c> is allowed.</summary>
    SockAccept = 0x20000000,
}
