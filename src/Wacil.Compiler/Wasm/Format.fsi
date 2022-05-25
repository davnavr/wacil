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

type Index = uint32

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

[<NoComparison; StructuralEquality>]
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
type NumType = I32 | I64 | F32 | F64

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type VecType = V128

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type RefType = FuncRef | ExternRef

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type ValType = | Num of n: NumType | Vec of v: VecType | Ref of r: RefType

type ResultType = ImmutableArray<ValType>

[<NoComparison; StructuralEquality>]
type FuncType = { Parameters: ResultType; Results: ResultType }

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type Limits =
    member Minimum: uint32
    member Maximum: uint32 voption
    
[<RequireQualifiedAccess>]
module Limits =
    val ofMin: min: uint32 -> Limits
    val tryWithMax: min: uint32 -> max: uint32 voption -> Limits voption

[<NoComparison; StructuralEquality>]
type TableType = { ElementType: RefType; Limits: Limits }

type MemType = Limits

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type Mutability = Const | Var

[<NoComparison; StructuralEquality>]
type GlobalType = { Type: ValType; Mutability: Mutability }

[<NoComparison; StructuralEquality>]
type ImportDesc =
    | Func of Index
    | Table of TableType
    | Mem of MemType
    | Global of GlobalType

[<NoComparison; StructuralEquality>]
type Import = { Module: string; Name: string; Description: ImportDesc }

[<NoComparison; StructuralEquality>]
type Section =
    | Custom of CustomSection
    | Type of ImmutableArray<FuncType>
    | Import of ImmutableArray<Import>
    | Function of types: ImmutableArray<Index>
    | Table of ImmutableArray<TableType>

type Module = ImmutableArray<Section>
