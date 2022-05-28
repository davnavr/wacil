//! Functions for compiling WebAssembly modules into CLI metadata as described in ECMA-335.
[<RequireQualifiedAccess>]
module Wacil.Compiler.Emit.Module

//val compileToModule

val compileToStream:
    options: Options ->
    input: Wacil.Compiler.Wasm.Validation.ValidModule ->
    stream: System.IO.Stream -> unit
