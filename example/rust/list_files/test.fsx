#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/Wacil.Runtime.Wasi.dll"
#r "./out/list_files.dll"

open Wacil.Runtime
open Wacil.Runtime.Wasi.Snapshot.Preview1

do
    try
        let logger = Logger()
        let mutable instance: list_files.list_files = null
        let memory = new LazyMemory<_>(lazy instance.memory)
        let env = EnvironmentVariables memory
        let file_descriptors = System.Collections.Generic.Dictionary()
        let standard_output = new System.IO.MemoryStream()
        file_descriptors[1] <- FileDescriptor standard_output
        let file_system = FileSystem(memory, file_descriptors)
        instance <-
            list_files.list_files(list_files.wasi_snapshot_preview1(
                fd_write = logger.FdWrite(Imports.FdWrite file_system),
                environ_get = logger.EnvironGet(Imports.EnvironGet env),
                environ_sizes_get = logger.EnvironSizesGet(Imports.EnvironSizesGet env),
                proc_exit = ProcessExit.Throw
            ))

        instance._start()
    with
    | :? ProcessExitException as e when e.ExitCode = 0 -> ()
