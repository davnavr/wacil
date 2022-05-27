module Wacil.Compiler.Emit.Module

open System.Reflection.Metadata

open Wacil.Compiler.Helpers

let compileToBlobBuilder (options: Options) (builder: BlobBuilder) =
    raise (System.NotImplementedException())
    ()

let compileToStream options (stream: System.IO.Stream) =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanWrite then invalidArg (nameof stream) "Destination stream must support writing"
        let builder = BlobBuilder()
        compileToBlobBuilder options builder
        for blob in builder.GetBlobs() do
            stream.Write(Span.readonly(System.MemoryExtensions.AsSpan(blob.GetBytes())))
    finally
        stream.Close()
