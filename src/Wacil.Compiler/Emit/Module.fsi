//! Functions for compiling WebAssembly modules into CLI metadata as described in ECMA-335.
[<RequireQualifiedAccess>]
module Wacil.Compiler.Emit.Module

open Wacil.Compiler.Wasm.Validation

val compileToBlobBuilder: options: Options -> input: ValidModule -> builder: System.Reflection.Metadata.BlobBuilder -> unit

val compileToStream: options: Options -> input: ValidModule -> stream: System.IO.Stream -> unit
