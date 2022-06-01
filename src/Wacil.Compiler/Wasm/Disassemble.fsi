/// Contains functions for printing the contents of a WebAssembly module in the WebAssembly Text Format (WAT).
/// Useful for investigating the contents of parsed modules.
module Wacil.Compiler.Wasm.Disassemble

open Wacil.Compiler.Wasm.Validation

val disassembleToWriter: input: ValidModule -> output: System.IO.TextWriter -> unit
