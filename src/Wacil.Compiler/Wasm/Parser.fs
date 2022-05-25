module Wacil.Compiler.Wasm.Parser

open System.Collections.Immutable
open System.IO

open Wacil.Compiler.Helpers.Collections

let parseFromStream (stream: Stream): Format.Module =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanRead then invalidArg (nameof stream) "The stream must support reading"
        let sections = ArrayBuilder<Format.Section>.Create()

        sections.ToImmutableArray()
    finally
        stream.Close()

let parseFromPath path = parseFromStream(File.OpenRead path)
