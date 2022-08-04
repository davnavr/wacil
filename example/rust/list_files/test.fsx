#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/Wacil.Runtime.Wasi.dll"
#r "./out/list_files.dll"

open Wacil.Runtime.Wasi.Snapshot.Preview1

do
    let logger = Logger()
    let instance =
        list_files.list_files(list_files.wasi_snapshot_preview1(
            fd_write = logger.FdWrite Stubs.FdWrite,
            environ_get = logger.EnvironGet Stubs.EnvironGet,
            environ_sizes_get = logger.EnvironSizesGet Stubs.EnvironSizesGet,
            proc_exit = ProcessExit.Throw
        ))
    ()
