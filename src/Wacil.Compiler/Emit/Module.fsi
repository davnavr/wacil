//! Functions for compiling WebAssembly modules into CLI metadata as described in ECMA-335.
[<RequireQualifiedAccess>]
module Wacil.Compiler.Emit.Module

open Wacil.Compiler.Wasm.Format

val compileToBlobBuilder: options: Options -> webAssemblyModule: Module -> builder: System.Reflection.Metadata.BlobBuilder -> unit

val compileToStream: options: Options -> webAssemblyModule: Module -> stream: System.IO.Stream -> unit
