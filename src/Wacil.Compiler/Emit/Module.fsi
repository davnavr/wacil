//! Functions for compiling WebAssembly modules into CLI metadata as described in ECMA-335.
[<RequireQualifiedAccess>]
module Wacil.Compiler.Emit.Module

val compileToBlobBuilder: options: Options -> builder: System.Reflection.Metadata.BlobBuilder -> unit

val compileToStream: options: Options -> stream: System.IO.Stream -> unit
