namespace Wacil.Runtime.Wasi.Snapshot.Preview1;

/// <summary>Represents the possible error codes returned by WASI functions.</summary>
public enum Errno : short {
    /// <summary>The operation completed successfully.</summary>
    Success = 0,
    /// <summary>The argument list is too long.</summary>
    TooBig = 1,
    /// <summary><c>acces</c></summary>
    PermissionDenied = 2,
    /// <summary><c>addrinuse</c></summary>
    AddressInUse = 3,
    /// <summary><c>addrnotavail</c></summary>
    AddressNotAvailable = 4,
    /// <summary><c>afnosupport</c></summary>
    AddressFamilyNotSupported = 5,
    /// <summary>A resource is unavailable, or an operation would block.</summary>
    Again = 6,
    /// <summary>Connection already in progress.</summary>
    Already = 7,
    /// <summary><c>badf</c></summary>
    BadFileDescriptor = 8,
    /// <summary><c>badmsg</c></summary>
    BadMessage = 9,
    /// <summary>A device or resource is busy.</summary>
    Busy = 10,
    /// <summary>The operation was cancelled (<c>canceled</c>).</summary>
    Cancelled = 11,
    /// <summary><c>child</c></summary>
    NoChildProcesses = 12,
    /// <summary><c>connaborted</c></summary>
    ConnectionAborted = 13,
    /// <summary><c>connrefused</c></summary>
    ConnectionRefused = 14,
    /// <summary><c>connreset</c></summary>
    ConnectionReset = 15,
    /// <summary>A resource deadlock would occur.</summary>
    Deadlock = 16,
    /// <summary><c>destaddrreq</c></summary>
    DestinationAddressRequired = 17,
    /// <summary>Mathematics argument out of domain of function (<c>dom</c>).</summary>
    OutOfDomain = 18,
    /// <summary><c>exist</c></summary>
    FileExists = 20,
    /// <summary>Bad address.</summary>
    Fault = 21,
    /// <summary><c>fbig</c></summary>
    FileTooLarge = 22,
    /// <summary><c>hostunreach</c></summary>
    HostUnreachable = 23,
    /// <summary><c>idrm</c></summary>
    IdentifierRemoved = 24,
    /// <summary><c>ilseq</c></summary>
    IllegalByteSequence = 25,
    /// <summary>Operation in progress.</summary>
    InProgress = 26,
    /// <summary><c>intr</c></summary>
    Interrupted = 27,
    /// <summary><c>inval</c></summary>
    InvalidArgument = 28,
    /// <summary>An I/O error occured</summary>
    IO = 29,
    /// <summary>Socket is connected (<c>isconn</c>).</summary>
    IsConnected = 30,
    /// <summary><c>isdir</c></summary>
    IsDirectory = 31,
    /// <summary>Too many levels of symbolic links.</summary>
    Loop = 32,
    /// <summary><c>mfile</c></summary>
    FileDescriptorTooLarge = 33,
    /// <summary><c>mlink</c></summary>
    TooManyLinks = 34,
    /// <summary><c>msgsize</c></summary>
    MessageTooLarge = 35,
    /// <summary>The file name is too long.</summary>
    NameTooLong = 37,
    /// <summary><c>netdown</c></summary>
    NetworkDown = 38,
    /// <summary>Connection aborted by network (<c>netreset</c>).</summary>
    NetworkReset = 39,
    /// <summary><c>netunreach</c></summary>
    NetworkUnreachable = 40,
    /// <summary><c>nfile</c></summary>
    TooManyFilesOpen = 41,
    /// <summary><c>nobufs</c></summary>
    NoBufferSpaceAvailable = 42,
    /// <summary>No such device (<c>nodev</c>).</summary>
    NoDevice = 43,
    /// <summary>No such file or directory (<c>noent</c>).</summary>
    NoFileOrDirectory = 44,
    /// <summary><c>noexec</c></summary>
    NotExecutable = 45,
    /// <summary><c>nolck</c></summary>
    NoLocksAvailable = 46,
    /// <summary><c>nomem</c></summary>
    NotEnoughSpace = 48,
    /// <summary>No message of the desired type</summary>
    NoMessage = 49,
    /// <summary><c>noprotoopt</c></summary>
    ProtocolNotAvailable = 50,
    /// <summary><c>nospc</c></summary>
    NoSpaceLeftOnDevice = 51,
    /// <summary><c>nosys</c></summary>
    FunctionNotSupported = 52,
    /// <summary>The socket is not connected.</summary>
    NotConnected = 53,
    /// <summary><c>notdir</c></summary>
    NotADirectory = 54,
    /// <summary>The directory is not empty.</summary>
    NotEmpty = 55,
    /// <summary>State is not recoverable.</summary>
    NotRecoverable = 56,
    /// <summary><c>notsock</c></summary>
    NotASocket = 57,
    /// <summary><c>notsup</c></summary>
    NotSupported = 58,
    /// <summary><c>notty</c></summary>
    InappropriateIOControlOperation = 59,
    /// <summary><c>nxio</c></summary>
    NoSuchDeviceOrAddress = 60,
    /// <summary>Value is too large to be stored in a particular data type.</summary>
    Overflow = 61,
    /// <summary>Previous owner died.</summary>
    OwnerDead = 62,
    /// <summary><c>perm</c></summary>
    OperationNotPermitted = 63,
    /// <summary><c>pipe</c></summary>
    BrokenPipe = 64,
    /// <summary><c>proto</c></summary>
    ProtocolError = 65,
    /// <summary><c>protonosupport</c></summary>
    ProtocolNotSupported = 66,
    /// <summary><c>prototype</c></summary>
    WrongProtocolType = 67,
    /// <summary><c>range</c></summary>
    ResultTooLarge = 68,
    /// <summary><c>rofs</c></summary>
    ReadOnlyFileSystem = 69,
    /// <summary><c>spipe</c></summary>
    InvalidSeek = 70,
    /// <summary><c>srch</c></summary>
    NoSuchProcess = 71,
    /// <summary><c>timedout</c></summary>
    ConnectionTimedOut = 73,
    /// <summary><c>txtbsy</c></summary>
    TextFileBusy = 74,
    /// <summary><c>xdev</c></summary>
    CrossDeviceLink = 75,
    /// <summary><c>notcapable</c></summary>
    InsufficientCapabilities = 76,
}
