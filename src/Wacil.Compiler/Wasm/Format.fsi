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

[<RequireQualifiedAccess>]
module ValType =
    val singleI32 : ImmutableArray<ValType>
    val singleI64 : ImmutableArray<ValType>

type ResultType = ImmutableArray<ValType>

[<NoComparison; StructuralEquality>]
type FuncType =
    { Parameters: ResultType
      Results: ResultType }

[<RequireQualifiedAccess>]
module FuncType =
    val empty : FuncType

    val ofReturnType: returnType: ValType -> FuncType

type Index = uint32

type Opcode =
    | Unreachable = 0uy
    | Nop = 1uy
    | Block = 2uy
    | Loop = 3uy
    | If = 4uy
    | Else = 5uy
    | End = 0xBuy
    | Br = 0xCuy
    | BrIf = 0xDuy
    | BrTable = 0xEuy
    | Return = 0xFuy
    | Call = 0x10uy
    | CallIndirect = 0x11uy
    | Drop = 0x1Auy
    | LocalGet = 0x20uy
    | LocalSet = 0x21uy
    | LocalTee = 0x22uy
    | GlobalGet = 0x23uy
    | GlobalSet = 0x24uy
    | I32Load = 0x28uy
    | I64Load = 0x29uy
    | F32Load = 0x2Auy
    | F64Load = 0x2Buy
    | I32Store = 0x36uy
    | I64Store = 0x37uy
    | F32Store = 0x38uy
    | F64Store = 0x39uy
    | MemoryGrow = 0x40uy
    | I32Const = 0x41uy
    | I64Const = 0x42uy
    | F32Const = 0x43uy
    | F64Const = 0x44uy
    | I32Eqz = 0x45uy
    | I32Eq = 0x46uy
    | I32Ne = 0x47uy
    | I32LtS = 0x48uy
    | I32LtU = 0x49uy
    | I32GtS = 0x4Auy
    | I32GtU = 0x4Buy
    | I32LeS = 0x4Cuy
    | I32LeU = 0x4Duy
    | I32GeS = 0x4Euy
    | I32GeU = 0x4Fuy
    | I64Eqz = 0x50uy
    | I64Eq = 0x51uy
    | I64Ne = 0x52uy
    | I64LtS = 0x53uy
    | I64LtU = 0x54uy
    | I64GtS = 0x55uy
    | I64GtU = 0x56uy
    | I64LeS = 0x57uy
    | I64LeU = 0x58uy
    | I64GeS = 0x59uy
    | I64GeU = 0x5Auy
    | I32Add = 0x6Auy
    | I32Sub = 0x6Buy
    | I32Mul = 0x6Cuy
    | I32DivS = 0x6Duy
    | I32DivU = 0x6Euy
    | I32RemS = 0x6Fuy
    | I32RemU = 0x70uy
    | I32And = 0x71uy
    | I64Add = 0x7Cuy
    | I64Sub = 0x7Duy
    | I64Mul = 0x7Euy
    | I64DivS = 0x7Fuy
    | I64DivU = 0x80uy
    | I64RemS = 0x81uy
    | I64RemU = 0x82uy

[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type MemArgAlignment =
    | MemArgAlignment of power: uint32

    member Power: uint32

    /// Gets the alignment, in bytes
    member Alignment: uint64

/// Immediate used by memory load and store instructions.
[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type MemArg = { Alignment: MemArgAlignment; Offset: uint32 }

// TODO: Or should this be FuncType? It seems multi-value proposal might have been merged?
[<IsReadOnly; Struct; RequireQualifiedAccess; NoComparison; StructuralEquality>]
type BlockType =
    | Void
    | Index of index: Index
    | Val of ValType
    //| Func of FuncType

/// Contains instructions that don't mark the start of nested blocks.
[<NoComparison; StructuralEquality>]
type Instruction =
    | Unreachable
    | Nop
    | Br of label: Index
    | BrIf of label: Index
    | Block of BlockType
    | Loop of BlockType
    | If of BlockType
    | Else
    | End
    | Drop
    | Call of callee: Index
    | CallIndirect of functionType: Index * table: Index
    | LocalGet of Index
    | LocalSet of Index
    | LocalTee of Index
    | GlobalGet of Index
    | GlobalSet of Index
    | I32Load of MemArg
    | I64Load of MemArg
    | F32Load of MemArg
    | F64Load of MemArg
    | I32Store of MemArg
    | I64Store of MemArg
    | F32Store of MemArg
    | F64Store of MemArg
    | MemoryGrow
    | I32Const of int32
    | I64Const of int64
    | F32Const of single
    | F64Const of double
    | I32Eqz
    | I32Eq
    | I32Ne
    | I32LtS
    | I32LtU
    | I32GtS
    | I32GtU
    | I32LeS
    | I32LeU
    | I32GeS
    | I32GeU
    | I64Eqz
    | I64Eq
    | I64Ne
    | I64LtS
    | I64LtU
    | I64GtS
    | I64GtU
    | I64LeS
    | I64LeU
    | I64GeS
    | I64GeU
    | I32Add
    | I32Sub
    | I32Mul
    | I32DivS
    | I32DivU
    | I32RemS
    | I32RemU
    | I32And
    | I64Add
    | I64Sub
    | I64Mul
    | I64DivS
    | I64DivU
    | I64RemS
    | I64RemU

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

[<NoComparison; StructuralEquality>]
type Custom = { Name: Name; Contents: ImmutableArray<byte> }

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

type Expression = ImmutableArray<Instruction>

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
