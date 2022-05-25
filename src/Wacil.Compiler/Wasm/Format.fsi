/// Model of the WebAssembly binary format.
module Wacil.Compiler.Wasm.Format

open System.Collections.Immutable
open System.Runtime.CompilerServices

/// Contains magic numbers used in the beginning of WebAssembly modules.
[<RequireQualifiedAccess>]
module Preamble =
    /// The magic number that all WebAssembly modules start with.
    val magic : ImmutableArray<byte>

    /// Indicates the version of the WebAssembly binary format.
    val version : ImmutableArray<byte>

type Name = string

[<NoComparison; NoEquality>]
type CustomSection = { Name: Name; Contents: ImmutableArray<byte> }

[<NoComparison; NoEquality>]
type Section =
    | CustomSection of CustomSection

type Module = ImmutableArray<Section>
