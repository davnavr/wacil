//! Functions for compiling WebAssembly modules into CLI metadata as described in ECMA-335.
[<RequireQualifiedAccess>]
module Wacil.Compiler.Emit.Module

open Wacil.Compiler.Wasm.Validation

val compileToModuleDefinition: options: Options -> input: ValidModule -> AsmResolver.DotNet.ModuleDefinition

val compileToStream: options: Options -> input: ValidModule -> stream: System.IO.Stream -> unit
