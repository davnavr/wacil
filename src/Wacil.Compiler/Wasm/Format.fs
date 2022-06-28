module Wacil.Compiler.Wasm.Format

open System.Collections.Immutable

open Wacil.Compiler.Helpers

module Preamble =
    let magic = Unsafe.Array.toImmutable [| 0uy; 0x61uy; 0x73uy; 0x6duy; |]
    let version = Unsafe.Array.toImmutable [| 1uy; 0uy; 0uy; 0uy; |]

type Index = uint32

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

module ValType =
    let singleI32 = ImmutableArray.Create(Num I32)
    let singleI64 = ImmutableArray.Create(Num I64)

type ResultType = ImmutableArray<ValType>

type FuncType = { Parameters: ResultType; Results: ResultType }

module FuncType =
    let empty = { Parameters = ImmutableArray.Empty; Results = ImmutableArray.Empty }

    let ofReturnType returnType = { Parameters = ImmutableArray.Empty; Results = ImmutableArray.Create(item = returnType) }

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
    | Select = 0x1Buy
    | SelectMany = 0x1Cuy
    | LocalGet = 0x20uy
    | LocalSet = 0x21uy
    | LocalTee = 0x22uy
    | GlobalGet = 0x23uy
    | GlobalSet = 0x24uy
    | TableGet = 0x25uy
    | TableSet = 0x26uy
    | I32Load = 0x28uy
    | I64Load = 0x29uy
    | F32Load = 0x2Auy
    | F64Load = 0x2Buy
    | I32Load8S = 0x2Cuy
    | I32Load8U = 0x2Duy
    | I32Load16S = 0x2Euy
    | I32Load16U = 0x2Fuy
    | I64Load8S = 0x30uy
    | I64Load8U = 0x31uy
    | I64Load16S = 0x32uy
    | I64Load16U = 0x33uy
    | I64Load32S = 0x34uy
    | I64Load32U = 0x35uy
    | I32Store = 0x36uy
    | I64Store = 0x37uy
    | F32Store = 0x38uy
    | F64Store = 0x39uy
    | I32Store8 = 0x3Auy
    | I32Store16 = 0x3Buy
    | I64Store8 = 0x3Cuy
    | I64Store16 = 0x3Duy
    | I64Store32 = 0x3Euy
    | MemorySize = 0x3Fuy
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
    | MemoryFill = 0xFCuy

[<Struct>]
type MemArgAlignment =
    | MemArgAlignment of power: uint32

    member this.Power = let (MemArgAlignment power) = this in power

    member this.Alignment = pown 2UL (int32 this.Power)

[<Struct>]
type MemArg = { Alignment: MemArgAlignment; Offset: uint32 }

// TODO: Or should this be FuncType? It seems multi-value proposal might have been merged?
[<Struct>]
type BlockType =
    | Void
    | Index of index: Index
    | Val of ValType
    //| Func of FuncType

type Instruction =
    | Unreachable
    | Nop
    | Block of BlockType
    | Loop of BlockType
    | If of BlockType
    | Else
    | End
    | Br of label: Index
    | BrIf of label: Index
    | BrTable of targetLabels: ImmutableArray<Index> * defaultLabel: Index
    | Return
    | Call of callee: Index
    | CallIndirect of functionType: Index * table: Index
    | Drop
    | Select
    | LocalGet of Index
    | LocalSet of Index
    | LocalTee of Index
    | GlobalGet of Index
    | GlobalSet of Index
    | TableGet of Index
    | TableSet of Index
    | I32Load of MemArg
    | I64Load of MemArg
    | F32Load of MemArg
    | F64Load of MemArg
    | I32Load8S of MemArg
    | I32Load8U of MemArg
    | I32Load16S of MemArg
    | I32Load16U of MemArg
    | I64Load8S of MemArg
    | I64Load8U of MemArg
    | I64Load16S of MemArg
    | I64Load16U of MemArg
    | I64Load32S of MemArg
    | I64Load32U of MemArg
    | I32Store of MemArg
    | I64Store of MemArg
    | F32Store of MemArg
    | F64Store of MemArg
    | I32Store8 of MemArg
    | I32Store16 of MemArg
    | I64Store8 of MemArg
    | I64Store16 of MemArg
    | I64Store32 of MemArg
    | MemorySize
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
    | MemoryInit of data: Index
    | DataDrop of Index
    | MemoryCopy
    | MemoryFill

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

type Custom = { Name: Name; Contents: ImmutableArray<byte> }

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
    | Func of ty: Index
    | Table of TableType
    | Mem of MemType
    | Global of GlobalType

type Import = { Module: string; Name: string; Description: ImportDesc }

type Expression = ImmutableArray<Instruction>

type Global = { Type: GlobalType; Expression: Expression }

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

type DataMode =
    | Passive
    | Active of memory: Index * offset: Expression

type Data  = { Bytes: ImmutableArray<byte>; Mode: DataMode }

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

    member this.Id =
        match this with
        | Custom _ -> SectionId.Custom
        | Type _ -> SectionId.Type
        | Import _ -> SectionId.Import
        | Function _ -> SectionId.Function
        | Table _ -> SectionId.Table
        | Memory _ -> SectionId.Memory
        | Global _ -> SectionId.Global
        | Export _ -> SectionId.Export
        | Start _ -> SectionId.Start
        | Element _ -> SectionId.Element
        | Code _ -> SectionId.Code
        | Data _ -> SectionId.Data
        | DataCount _ -> SectionId.DataCount

type Module = ImmutableArray<Section>
