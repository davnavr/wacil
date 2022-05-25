module Wacil.Compiler.Wasm.Parser

open System.IO

let parseFromStream (stream: #Stream): Format.Module =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanRead then invalidArg (nameof stream) "The stream must support reading"
        raise (System.NotImplementedException())
    finally
        stream.Close()

let fromPath path = parseFromStream(File.OpenRead path)
