#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/Wacil.Runtime.Wasi.dll"
#r "./out/list_files.dll"

open Wacil.Runtime
open Wacil.Runtime.Wasi.Snapshot.Preview1

do
    try
        let logger = Logger()
        let env = EnvironmentVariables<ArrayMemory>()
        let instance =
            list_files.list_files(list_files.wasi_snapshot_preview1(
                fd_write = logger.FdWrite Stubs.FdWrite,
                environ_get = logger.EnvironGet(Imports.EnvironGet env),
                environ_sizes_get = logger.EnvironSizesGet(Imports.EnvironSizesGet env),
                proc_exit = ProcessExit.Throw
            ))

        instance._start()
    with
    | :? ProcessExitException as e when e.ExitCode = 0 -> ()
