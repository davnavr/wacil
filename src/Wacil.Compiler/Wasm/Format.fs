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

[<RequireQualifiedAccess>]
type ValType = | Num of n: NumType | Vec of v: VecType | Ref of r: RefType

module ValType =
    let i32 = ValType.Num I32
    let i64 = ValType.Num I64
    let f32 = ValType.Num F32
    let f64 = ValType.Num F64
    let v128 = ValType.Vec V128
    let funcref = ValType.Ref FuncRef
    let externref = ValType.Ref ExternRef

    let singleI32 = ImmutableArray.Create i32 
    let singleI64 = ImmutableArray.Create i64

    let ofNumType n =
        match n with
        | I32 -> i32
        | I64 -> i64
        | F32 -> f32
        | F64 -> f64

    let ofRefType r =
        match r with
        | FuncRef -> funcref
        | ExternRef -> externref

    let ofVecType v = ValType.Vec v

type ResultType = ImmutableArray<ValType>

type FuncType = { Parameters: ResultType; Results: ResultType }

module FuncType =
    let empty = { Parameters = ImmutableArray.Empty; Results = ImmutableArray.Empty }

    let ofReturnType returnType = { Parameters = ImmutableArray.Empty; Results = ImmutableArray.Create(item = returnType) }

[<Struct>]
type TypeIdx =
    | TypeIdx of int

    static member From (index: int64) = TypeIdx(Checked.int32 index)
    static member From (index: uint64) = TypeIdx(Checked.int32 index)
    static member inline op_Implicit(TypeIdx index) = index

[<Struct>]
type FuncIdx =
    | FuncIdx of int

    static member From (index: int64) = FuncIdx(Checked.int32 index)
    static member From (index: uint64) = FuncIdx(Checked.int32 index)
    static member inline op_Implicit(FuncIdx index) = index

[<Struct>]
type TableIdx =
    | TableIdx of int

    static member From (index: int64) = TableIdx(Checked.int32 index)
    static member From (index: uint64) = TableIdx(Checked.int32 index)
    static member inline op_Implicit(TableIdx index) = index

[<Struct>]
type MemIdx =
    | MemIdx of int

    static member inline Zero = MemIdx 0
    static member From (index: int64) = MemIdx(Checked.int32 index)
    static member From (index: uint64) = MemIdx(Checked.int32 index)
    static member inline op_Implicit(MemIdx index) = index

[<Struct>]
type GlobalIdx =
    | GlobalIdx of int

    static member From (index: int64) = GlobalIdx(Checked.int32 index)
    static member From (index: uint64) = GlobalIdx(Checked.int32 index)
    static member inline op_Implicit(GlobalIdx index) = index

[<Struct>]
type ElemIdx =
    | ElemIdx of int

    static member From (index: int64) = ElemIdx(Checked.int32 index)
    static member From (index: uint64) = ElemIdx(Checked.int32 index)
    static member inline op_Implicit(ElemIdx index) = index

[<Struct>]
type DataIdx =
    | DataIdx of int

    static member From (index: int64) = DataIdx(Checked.int32 index)
    static member From (index: uint64) = DataIdx(Checked.int32 index)
    static member inline op_Implicit(DataIdx index) = index

[<Struct>]
type LocalIdx =
    | LocalIdx of int

    static member From (index: int64) = LocalIdx(Checked.int32 index)
    static member From (index: uint64) = LocalIdx(Checked.int32 index)
    static member inline op_Implicit(LocalIdx index) = index

[<Struct>]
type LabelIdx =
    | LabelIdx of int

    static member From (index: int64) = LabelIdx(Checked.int32 index)
    static member From (index: uint64) = LabelIdx(Checked.int32 index)
    static member inline op_Implicit(LabelIdx index) = index

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
    | F32Eq = 0x5Buy
    | F32Ne = 0x5Cuy
    | F32Lt = 0x5Duy
    | F32Gt = 0x5Euy
    | F32Le = 0x5Fuy
    | F32Ge = 0x60uy
    | F64Eq = 0x61uy
    | F64Ne = 0x62uy
    | F64Lt = 0x63uy
    | F64Gt = 0x64uy
    | F64Le = 0x65uy
    | F64Ge = 0x66uy
    | I32Clz = 0x67uy
    | I32Ctz = 0x68uy
    | I32Popcnt = 0x69uy
    | I32Add = 0x6Auy
    | I32Sub = 0x6Buy
    | I32Mul = 0x6Cuy
    | I32DivS = 0x6Duy
    | I32DivU = 0x6Euy
    | I32RemS = 0x6Fuy
    | I32RemU = 0x70uy
    | I32And = 0x71uy
    | I32Or = 0x72uy
    | I32Xor = 0x73uy
    | I32Shl = 0x74uy
    | I32ShrS = 0x75uy
    | I32ShrU = 0x76uy
    | I32Rotl = 0x77uy
    | I32Rotr = 0x78uy
    | I64Clz = 0x79uy
    | I64Ctz = 0x7Auy
    | I64Popcnt = 0x7Buy
    | I64Add = 0x7Cuy
    | I64Sub = 0x7Duy
    | I64Mul = 0x7Euy
    | I64DivS = 0x7Fuy
    | I64DivU = 0x80uy
    | I64RemS = 0x81uy
    | I64RemU = 0x82uy
    | I64And = 0x83uy
    | I64Or = 0x84uy
    | I64Xor = 0x85uy
    | I64Shl = 0x86uy
    | I64ShrS = 0x87uy
    | I64ShrU = 0x88uy
    | I64Rotl = 0x89uy
    | I64Rotr = 0x8Auy
    | F32Abs = 0x8Buy
    | F32Neg = 0x8Cuy
    | F32Ceil = 0x8Duy
    | F32Floor = 0x8Euy
    | F32Trunc = 0x8Fuy
    | F32Nearest = 0x90uy
    | F32Sqrt = 0x91uy
    | F32Add = 0x92uy
    | F32Sub = 0x93uy
    | F32Mul = 0x94uy
    | F32Div = 0x95uy
    | F32Min = 0x96uy
    | F32Max = 0x97uy
    | F32Copysign = 0x98uy
    | F64Abs = 0x99uy
    | F64Neg = 0x9Auy
    | F64Ceil = 0x9Buy
    | F64Floor = 0x9Cuy
    | F64Trunc = 0x9Duy
    | F64Nearest = 0x9Euy
    | F64Sqrt = 0x9Fuy
    | F64Add = 0xA0uy
    | F64Sub = 0xA1uy
    | F64Mul = 0xA2uy
    | F64Div = 0xA3uy
    | F64Min = 0xA4uy
    | F64Max = 0xA5uy
    | F64Copysign = 0xA6uy
    | I32WrapI64 = 0xA7uy
    | I32TruncF32S = 0xA8uy
    | I32TruncF32U = 0xA9uy
    | I32TruncF64S = 0xAAuy
    | I32TruncF64U = 0xABuy
    | I64ExtendI32S = 0xACuy
    | I64ExtendI32U = 0xADuy
    | I64TruncF32S = 0xAEuy
    | I64TruncF32U = 0xAFuy
    | I64TruncF64S = 0xB0uy
    | I64TruncF64U = 0xB1uy
    | F32ConvertI32S = 0xB2uy
    | F32ConvertI32U = 0xB3uy
    | F32ConvertI64S = 0xB4uy
    | F32ConvertI64U = 0xB5uy
    | F32DemoteF64 = 0xB6uy
    | F64ConvertI32S = 0xB7uy
    | F64ConvertI32U = 0xB8uy
    | F64ConvertI64S = 0xB9uy
    | F64ConvertI64U = 0xBAuy
    | F64PromoteF32 = 0xBBuy
    | I32ReinterpretF32 = 0xBCuy
    | I64ReinterpretF64 = 0xBDuy
    | F32ReinterpretI32 = 0xBEuy
    | F64ReinterpretI64 = 0xBFuy
    | I32Extend8S = 0xC0uy
    | I32Extend16S = 0xC1uy
    | I64Extend8S = 0xC2uy
    | I64Extend16S = 0xC3uy
    | I64Extend32S = 0xC4uy
    | RefNull = 0xD0uy
    | RefIsNull = 0xD1uy
    | RefFunc = 0xD2uy
    | PrefixFC = 0xFCuy

[<Struct>]
type MemArgAlign =
    | MemArgAlign of power: uint8

    member this.Power = let (MemArgAlign power) = this in power

    member this.Alignment = pown 2UL (int32 this.Power)

[<Struct>]
type MemArg =
    { Alignment: MemArgAlign
      Offset: uint32
      Memory: MemIdx }

// TODO: Or should this be FuncType? It seems multi-value proposal might have been merged?
[<Struct>]
type BlockType =
    | Void
    | Index of index: TypeIdx
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
    | Br of label: LabelIdx
    | BrIf of label: LabelIdx
    | BrTable of targetLabels: ImmutableArray<LabelIdx> * defaultLabel: LabelIdx
    | Return
    | Call of callee: FuncIdx
    | CallIndirect of functionType: TypeIdx * table: TableIdx
    | Drop
    | Select
    | LocalGet of LocalIdx
    | LocalSet of LocalIdx
    | LocalTee of LocalIdx
    | GlobalGet of GlobalIdx
    | GlobalSet of GlobalIdx
    | TableGet of TableIdx
    | TableSet of TableIdx
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
    | F32Eq
    | F32Ne
    | F32Lt
    | F32Gt
    | F32Le
    | F32Ge
    | F64Eq
    | F64Ne
    | F64Lt
    | F64Gt
    | F64Le
    | F64Ge
    | I32Clz
    | I32Ctz
    | I32Popcnt
    | I32Add
    | I32Sub
    | I32Mul
    | I32DivS
    | I32DivU
    | I32RemS
    | I32RemU
    | I32And
    | I32Or
    | I32Xor
    | I32Shl
    | I32ShrS
    | I32ShrU
    | I32Rotl
    | I32Rotr
    | I64Clz
    | I64Ctz
    | I64Popcnt
    | I64Add
    | I64Sub
    | I64Mul
    | I64DivS
    | I64DivU
    | I64RemS
    | I64RemU
    | I64And
    | I64Or
    | I64Xor
    | I64Shl
    | I64ShrS
    | I64ShrU
    | I64Rotl
    | I64Rotr
    | F32Abs
    | F32Neg
    | F32Ceil
    | F32Floor
    | F32Trunc
    | F32Nearest
    | F32Sqrt
    | F32Add
    | F32Sub
    | F32Mul
    | F32Div
    | F32Min
    | F32Max
    | F32Copysign
    | F64Abs
    | F64Neg
    | F64Ceil
    | F64Floor
    | F64Trunc
    | F64Nearest
    | F64Sqrt
    | F64Add
    | F64Sub
    | F64Mul
    | F64Div
    | F64Min
    | F64Max
    | F64Copysign
    | I32WrapI64
    | I32TruncF32S
    | I32TruncF32U
    | I32TruncF64S
    | I32TruncF64U
    | I64ExtendI32S
    | I64ExtendI32U
    | I64TruncF32S
    | I64TruncF32U
    | I64TruncF64S
    | I64TruncF64U
    | F32ConvertI32S
    | F32ConvertI32U
    | F32ConvertI64S
    | F32ConvertI64U
    | F32DemoteF64
    | F64ConvertI32S
    | F64ConvertI32U
    | F64ConvertI64S
    | F64ConvertI64U
    | F64PromoteF32
    | I32ReinterpretF32
    | I64ReinterpretF64
    | F32ReinterpretI32
    | F64ReinterpretI64
    | I32Extend8S
    | I32Extend16S
    | I64Extend8S
    | I64Extend16S
    | I64Extend32S
    | RefNull of RefType
    | RefIsNull
    | RefFunc of func: FuncIdx
    | I32TruncSatF32S
    | I32TruncSatF32U
    | I32TruncSatF64S
    | I32TruncSatF64U
    | I64TruncSatF32S
    | I64TruncSatF32U
    | I64TruncSatF64S
    | I64TruncSatF64U
    | MemoryInit of data: DataIdx
    | DataDrop of DataIdx
    | MemoryCopy
    | MemoryFill
    | TableInit of element: ElemIdx * table: TableIdx
    | ElemDrop of element: ElemIdx
    | TableCopy of table1: TableIdx * table2: TableIdx
    | TableGrow of table: TableIdx
    | TableSize of table: TableIdx
    | TableFill of table: TableIdx

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
    | Func of TypeIdx
    | Table of TableType
    | Mem of MemType
    | Global of GlobalType

type Import = { Module: string; Name: string; Description: ImportDesc }

type Expression = ImmutableArray<Instruction>

type Global = { Type: GlobalType; Expression: Expression }

type ExportDesc =
    | Func of FuncIdx
    | Table of TableIdx
    | Mem of MemIdx
    | Global of GlobalIdx

type Export = { Name: string; Description: ExportDesc }

type ElementMode =
    | Passive
    | Active of table: TableIdx * offset: Expression
    | Declarative

type Element = { Type: RefType; Expressions: ImmutableArray<Expression>; Mode: ElementMode }

[<Struct>]
type Local = { Count: uint32; Type: ValType }

type Code = { Locals: ImmutableArray<Local>; Body: Expression }

type DataMode =
    | Passive
    | Active of memory: MemIdx * offset: Expression

type Data  = { Bytes: ImmutableArray<byte>; Mode: DataMode }

type Section =
    | Custom of Custom
    | Type of ImmutableArray<FuncType>
    | Import of ImmutableArray<Import>
    | Function of types: ImmutableArray<TypeIdx>
    | Table of ImmutableArray<TableType>
    | Memory of ImmutableArray<Limits>
    | Global of ImmutableArray<Global>
    | Export of ImmutableArray<Export>
    | Start of FuncIdx
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
