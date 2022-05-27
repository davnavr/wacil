module Wacil.Compiler.Emit.Module

open System.Reflection.Metadata

open Wacil.Compiler.Helpers
open Wacil.Compiler.Wasm

let compileToBlobBuilder (options: Options) (webAssemblyModule: Format.Module) (builder: BlobBuilder) =
    raise (System.NotImplementedException())
    ()

let compileToStream options webAssemblyModule (stream: System.IO.Stream) =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanWrite then invalidArg (nameof stream) "Destination stream must support writing"
        let builder = BlobBuilder()
        compileToBlobBuilder options webAssemblyModule builder
        builder.WriteContentTo(stream)
    finally
        stream.Close()
