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

type Opcode =
    | Unreachable = 0uy
    | Nop = 1uy
    | End = 0xBuy
    | I32Load = 0x28uy
    | I64Load = 0x29uy
    | F32Load = 0x2Auy
    | F64Load = 0x2Buy
    | MemoryGrow = 0x40uy
    | I32Const = 0x41uy
    | I64Const = 0x42uy
    | F32Const = 0x43uy
    | F64Const = 0x44uy

[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type MemArgAlignment =
    | MemArgAlignment of power: uint32

    member Power: uint32

    /// Gets the alignment, in bytes
    member Alignment: uint64

/// Immediate used by memory load and store instructions.
[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type MemArg = { Alignment: MemArgAlignment; Offset: uint32 }

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type Instruction =
    | Unreachable
    | Nop
    | I32Load of MemArg
    | I64Load of MemArg
    | F32Load of MemArg
    | F64Load of MemArg
    | MemoryGrow
    | I32Const of int32
    | I64Const of int64
    | F32Const of single
    | F64Const of double

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
type Custom = { Name: Name; Contents: ImmutableArray<byte> }

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

[<RequireQualifiedAccess; IsReadOnly; Struct; NoComparison; StructuralEquality>]
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

[<RequireQualifiedAccess; IsReadOnly; Struct; NoComparison; StructuralEquality>]
type Mutability = Const | Var

[<NoComparison; StructuralEquality>]
type GlobalType = { Type: ValType; Mutability: Mutability }

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type ImportDesc =
    | Func of ty: Index
    | Table of TableType
    | Mem of MemType
    | Global of GlobalType

[<NoComparison; StructuralEquality>]
type Import = { Module: string; Name: string; Description: ImportDesc }

/// <summary>An expression is a sequence of instructions terminated with an <c>end</c> isntruction.</summary>
[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type Expression = internal Expr of ImmutableArray<Instruction>

val (|Expression|): expression: Expression -> ImmutableArray<Instruction>

[<NoComparison; StructuralEquality>]
type Global = { Type: GlobalType; Expression: Expression }

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type ExportDesc =
    | Func of Index
    | Table of Index
    | Mem of Index
    | Global of Index

[<NoComparison; StructuralEquality>]
type Export = { Name: string; Description: ExportDesc }

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type ElementMode =
    | Passive
    | Active of table: Index * offset: Expression
    | Declarative

[<NoComparison; StructuralEquality>]
type Element = { Type: RefType; Expressions: Expression; Mode: ElementMode }

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type Local = { Count: uint32; Type: ValType }

[<NoComparison; StructuralEquality>]
type Code = { Locals: ImmutableArray<Local>; Body: Expression }

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type DataMode =
    | Passive
    | Active of memory: Index * offset: Expression

[<NoComparison; StructuralEquality>]
type Data  = { Bytes: ImmutableArray<byte>; Mode: DataMode }

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type Section =
    | Custom of Custom
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
    | Data of ImmutableArray<Data>
    | DataCount of count: uint32

    member Id: SectionId

type Module = ImmutableArray<Section>
