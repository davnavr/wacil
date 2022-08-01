#r "./out/Wacil.Runtime.dll"
#r "./out/Wacil.Runtime.Wasi.dll"
#r "./out/hello_wasi.dll"

open Wacil.Runtime.Wasi.Snapshot.Preview1
open Wacil.Runtime

do
    let memory = new ArrayMemory(Limits(1))
    let args = CommandLineArguments(memory)
    args.Add "hello"
    args.Add "there"

    let fdescriptors = System.Collections.Generic.Dictionary(1)
    let fs = FileSystem(memory, fdescriptors)
    use fdout = new System.IO.MemoryStream(64)
    fdescriptors[1] <- FileDescriptor fdout

    let wasi =
        hello_wasi.wasi_snapshot_preview1(
            fd_write = Imports.FdWrite(fs),
            proc_exit = ProcessExit.Throw,
            args_get = Imports.ArgsGet(args),
            args_sizes_get = Imports.ArgsSizesGet(args)
        )

    let instance = hello_wasi.hello_wasi(hello_wasi.env(memory), wasi)
    instance._initialize()
    instance.hello()
