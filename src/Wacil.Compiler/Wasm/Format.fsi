/// Model of the WebAssembly binary format.
[<RequireQualifiedAccess>]
module Wacil.Compiler.Wasm.Format

open System.Collections.Immutable

/// Contains magic numbers used in the beginning of WebAssembly modules.
[<RequireQualifiedAccess>]
module Preamble =
    /// The magic number that all WebAssembly modules start with.
    val magic : ImmutableArray<byte>
    /// Indicates the version of the WebAssembly binary format.
    val version : ImmutableArray<byte>
