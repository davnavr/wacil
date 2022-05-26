module Wacil.Compiler.Wasm.Format

open System.Collections.Immutable

open Wacil.Compiler.Helpers

module Preamble =
    let magic = Unsafe.Array.toImmutable [| 0uy; 0x61uy; 0x73uy; 0x6duy; |]
    let version = Unsafe.Array.toImmutable [| 1uy; 0uy; 0uy; 0uy; |]

type Opcode =
    | Unreachable = 0uy
    | Nop = 1uy

[<NoComparison; StructuralEquality>]
type Instruction =
    | Unreachable
    | Nop

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

type CustomSection = { Name: Name; Contents: ImmutableArray<byte> }

type Type =
    | ExternRef = 0x6Fuy
    | FuncRef = 0x70uy
    | V128 = 0x7Buy
    | I32 = 0x7Fuy
    | I64 = 0x7Euy
    | F32 = 0x7Duy
    | F64 = 0x7Cuy

[<Struct>]
type NumType = I32 | I64 | F32 | F64

[<Struct>]
type VecType = V128

[<Struct>]
type RefType = FuncRef | ExternRef

[<Struct>]
type ValType = | Num of n: NumType | Vec of v: VecType | Ref of r: RefType

type ResultType = ImmutableArray<ValType>

type FuncType = { Parameters: ResultType; Results: ResultType }

[<Struct>]
type Limits (minimum: uint32, maximum: uint32 voption) =
    member _.Minimum = minimum
    member _.Maximum = maximum

module Limits =
    let ofMin min = Limits(min, ValueNone)

    let tryWithMax min max =
        match max with
        | ValueSome max' when max' < min -> ValueNone
        | _ -> ValueSome(Limits(min, max))


type TableType = { ElementType: RefType; Limits: Limits }

type MemType = Limits

[<Struct>]
type Mutability = Const | Var

type GlobalType = { Type: ValType; Mutability: Mutability }

type ImportDesc =
    | Func of Index
    | Table of TableType
    | Mem of MemType
    | Global of GlobalType

type Import = { Module: string; Name: string; Description: ImportDesc }

type Expression = ImmutableArray<Instruction>

type Global = { Type: GlobalType; Expression: ImmutableArray<Instruction> }

type ExportDesc =
    | Func of Index
    | Table of Index
    | Mem of Index
    | Global of Index

type Export = { Name: string; Description: ExportDesc }

type ElementMode =
    | Passive
    | Active of table: Index * offset: Expression
    | Declarative

type Element = { Type: RefType; Expressions: Expression; Mode: ElementMode }

[<Struct>]
type Local = { Count: uint32; Type: ValType }

type Code = { Locals: ImmutableArray<Local>; Body: Expression }

type Section =
    | Custom of CustomSection
    | Type of ImmutableArray<FuncType>
    | Import of ImmutableArray<Import>
    | Function of types: ImmutableArray<Index>
    | Table of ImmutableArray<TableType>
    | Memory of ImmutableArray<Limits>
    | Global of ImmutableArray<Global>
    | Export of ImmutableArray<Export>
    | Start of Index
    | Element of ImmutableArray<Element>
    | Code of ImmutableArray<Code>

type Module = ImmutableArray<Section>
