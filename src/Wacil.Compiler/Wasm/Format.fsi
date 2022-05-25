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

[<NoComparison; NoEquality>]
type CustomSection = { Name: Name; Contents: ImmutableArray<byte> }

type Type =
    | ExternRef = 0x6Fuy
    | FuncRef = 0x70uy
    | V128 = 0x7Buy
    | I32 = 0x7Fuy
    | I64 = 0x7Euy
    | F32 = 0x7Duy
    | F64 = 0x7Cuy

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type ValueType = I32 | I64 | F32 | F64 | V128 | FuncRef | ExternRef

type ResultType = ImmutableArray<ValueType>

type FunctionType = { Parameters: ResultType; Results: ResultType }

[<NoComparison; NoEquality>]
type Section =
    | Custom of CustomSection
    | Type of ImmutableArray<FunctionType>

type Module = ImmutableArray<Section>
