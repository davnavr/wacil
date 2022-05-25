module Wacil.Compiler.Wasm.Format

open System.Collections.Immutable

open Wacil.Compiler.Helpers

module Preamble =
    let magic = Unsafe.Array.toImmutable [| 0uy; 0x61uy; 0x73uy; 0x6duy; |]
    let version = Unsafe.Array.toImmutable [| 1uy; 0uy; 0uy; 0uy; |]

type Name = string

type SectionId =
    | Custom = 0uy
    | Type = 1uy
    | Import = 2uy
    | Function = 3uy
    | Table = 4uy
    | Memory = 5uy
    | Global = 6uy
    | Export = 7uy
    | Start = 8uy
    | Element = 9uy
    | Code = 10uy
    | Data = 11uy
    | DataCount = 12uy
    
type CustomSection = { Name: Name; Contents: ImmutableArray<byte> }

type Section =
    | CustomSection of CustomSection

type Module = ImmutableArray<Section>
