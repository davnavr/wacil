module Wasm.Format

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Immutable

module Preamble =
    let magic = ImmutableArray.Create<byte> "\000asm"B
    let version = ImmutableArray.Create<byte> [| 1uy; 0uy; 0uy; 0uy |]

let [<Literal>] PageSize = 65536u

[<Struct>]
type MemSize (multiple: uint16) =
    member _.Size = uint32 multiple * uint32 PageSize

    static member inline op_Implicit(size: MemSize) = size.Size
    static member inline op_Explicit(size: MemSize) = Checked.int32 size.Size

module MemSize =
    let ofMultiple multiple = MemSize multiple

[<AutoOpen>]
module Types =
    type WasmType =
        | externref = 0x6Fuy
        | funcref = 0x70uy
        | i32 = 0x7Fuy
        | i64 = 0x7Euy
        | f32 = 0x7Duy
        | f64 = 0x7Cuy

    type NumType =
        | I32
        | I64
        | F32
        | F64

    type RefType =
        | FuncRef
        | ExternRef

    [<Struct>]
    type ValType =
        | NumType of numType: NumType
        | RefType of refType: RefType

    type ResultType = ImmutableArray<ValType>

    [<Struct>]
    type FuncType = { Parameters: ResultType; Results: ResultType }

    [<Sealed>]
    type Limit<'T when 'T : struct and 'T : equality> (min, max) =
        member _.Min: 'T = min
        member _.Max: 'T voption = max

        interface IEquatable<Limit<'T>> with member _.Equals other = min = other.Min && max = other.Max

    [<Struct>]
    type TableType = { ElementType: RefType; Limit: Limit<uint32> }

    type MemType = Limit<MemSize>

    type GlobalType =
        | Const of ValType
        | Var of ValType

    module Limit =
        let tryWithMax min max =
            match max with
            | ValueSome max' when max' < min -> ValueNone
            | _ -> ValueSome(Limit(min, max))

type Name = string

type CustomSection = { Name: Name; Contents: ImmutableArray<byte> }

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

module IndexKinds =
    [<AbstractClass>]
    type Kind () = class end

    [<AbstractClass; Sealed>]
    type Type private () = inherit Kind()

    [<AbstractClass; Sealed>]
    type Func private () = inherit Kind()

    [<AbstractClass; Sealed>]
    type Table private () = inherit Kind()

    [<AbstractClass; Sealed>]
    type Mem private () = inherit Kind()

    [<AbstractClass; Sealed>]
    type Global private () = inherit Kind()

    [<AbstractClass; Sealed>]
    type Elem private () = inherit Kind()

    [<AbstractClass; Sealed>]
    type Data private () = inherit Kind()

    [<AbstractClass; Sealed>]
    type Local private () = inherit Kind()

    [<AbstractClass; Sealed>]
    type Label private () = inherit Kind()

[<Struct>]
type Index<'Class when 'Class :> IndexKinds.Kind> =
    val Index: uint32

    new (index) = { Index = index }

    static member inline Zero = Index<'Class> 0u
    static member inline op_Implicit(index: Index<'Class>) = index.Index
    static member inline op_Explicit(index: Index<'Class>) = Checked.int32 index.Index

let inline (|Index|) (index: Index<_>) = index.Index

module InstructionSet =
    type Opcode = uint8

    [<Struct>]
    type MemArgAlignment (alignment: uint32) =
        member _.Alignment = alignment

        static member inline op_Implicit(alignment: MemArgAlignment) = alignment.Alignment

    let inline (|MemArgAlignment|) (alignment: MemArgAlignment) = alignment.Alignment

    module MemArgAlignment =
        let ofPower (power: uint8) = MemArgAlignment(pown 2u (int32 power))

    [<Struct>]
    type MemArg = { Alignment: MemArgAlignment; Offset: uint32 }

    [<RequireQualifiedAccess>]
    type BlockType =
        | Empty
        | ValueType of Types.ValType
        | Index of Index<IndexKinds.Type>

    [<RequireQualifiedAccess>]
    type InstructionArguments =
        | Nothing
        | Bytes of ImmutableArray<byte>
        | RefType of RefType
        | BlockType of BlockType
        | ValTypeVector of ImmutableArray<ValType>
        | FuncIndex of Index<IndexKinds.Func>
        | IndirectCall of Index<IndexKinds.Type> * Index<IndexKinds.Table>
        | LocalIndex of Index<IndexKinds.Local>
        | GlobalIndex of Index<IndexKinds.Global>
        | TableIndex of Index<IndexKinds.Table>
        | LabelIndex of Index<IndexKinds.Label>
        | LabelIndexVector of ImmutableArray<Index<IndexKinds.Label>>
        | MemArg of MemArg
        | I32 of int32
        | I64 of int64
        | F32 of Single
        | F64 of Double

    [<Struct>]
    type Instruction = { Opcode: Opcode; Arguments: InstructionArguments }

    let inline private simple opcode = { Opcode = opcode; Arguments = InstructionArguments.Nothing }

    module Control =
        let unreachable = simple 0uy
        let nop = simple 1uy
        let block btype = { Opcode = 2uy; Arguments = InstructionArguments.BlockType btype }
        let loop btype = { Opcode = 3uy; Arguments = InstructionArguments.BlockType btype }
        let ``if`` btype = { Opcode = 4uy; Arguments = InstructionArguments.BlockType btype }
        let ``else`` = simple 5uy
        let ``end`` = simple 0xBuy
        let br label = { Opcode = 0xCuy; Arguments = InstructionArguments.LabelIndex label }
        let br_if label = { Opcode = 0xDuy; Arguments = InstructionArguments.LabelIndex label }
        let br_table labels = { Opcode = 0xEuy; Arguments = InstructionArguments.LabelIndexVector labels }
        let ``return`` = simple 0xFuy
        let call f = { Opcode = 0x10uy; Arguments = InstructionArguments.FuncIndex f }
        let call_indirect y x = { Opcode = 0x10uy; Arguments = InstructionArguments.IndirectCall(y, x) }

    module Ref =
        let ``null`` t = { Opcode = 0xD0uy; Arguments = InstructionArguments.RefType t }
        let is_null = simple 0xD1uy
        let func x = { Opcode = 0xD2uy; Arguments = InstructionArguments.FuncIndex x }

    module Parametric =
        let drop = simple 0x1Auy
        let private select_none = simple 0x1Buy
        let select types =
            match types with
            | ValueNone -> select_none
            | ValueSome types' -> { Opcode = 0x1Cuy; Arguments = InstructionArguments.ValTypeVector types' }

    module Local =
        let ``get`` variable = { Opcode = 0x20uy; Arguments = InstructionArguments.LocalIndex variable }
        let ``set`` variable = { Opcode = 0x21uy; Arguments = InstructionArguments.LocalIndex variable }
        let tee variable = { Opcode = 0x22uy; Arguments = InstructionArguments.LocalIndex variable }

    module Global =
        let ``get`` variable = { Opcode = 0x23uy; Arguments = InstructionArguments.GlobalIndex variable }
        let ``set`` variable = { Opcode = 0x24uy; Arguments = InstructionArguments.GlobalIndex variable }

    module Table =
        let ``get`` table = { Opcode = 0x25uy; Arguments = InstructionArguments.TableIndex table }
        let ``set`` table = { Opcode = 0x26uy; Arguments = InstructionArguments.TableIndex table }


    //module Elem

    module I32 =
        let load m = { Opcode = 0x28uy; Arguments = InstructionArguments.MemArg m }
        let load8_s m = { Opcode = 0x2Cuy; Arguments = InstructionArguments.MemArg m }
        let load8_u m = { Opcode = 0x2Duy; Arguments = InstructionArguments.MemArg m }
        let load16_s m = { Opcode = 0x2Euy; Arguments = InstructionArguments.MemArg m }
        let load16_u m = { Opcode = 0x2Fuy; Arguments = InstructionArguments.MemArg m }
        let store m = { Opcode = 0x36uy; Arguments = InstructionArguments.MemArg m }
        let store8 m = { Opcode = 0x3Auy; Arguments = InstructionArguments.MemArg m }
        let store16 m = { Opcode = 0x3Buy; Arguments = InstructionArguments.MemArg m }
        let ``const`` n = { Opcode = 0x31uy; Arguments = InstructionArguments.I32 n }
        let eqz = simple 0x45uy
        let eq = simple 0x46uy
        let ne = simple 0x47uy
        let lt_s = simple 0x48uy
        let lt_u = simple 0x49uy
        let gt_s = simple 0x4Auy
        let gt_u = simple 0x4Buy
        let le_s = simple 0x4Cuy
        let le_u = simple 0x4Duy
        let ge_s = simple 0x4Euy
        let ge_u = simple 0x4Fuy
        let clz = simple 0x67uy
        let ctz = simple 0x68uy
        let popcnt = simple 0x69uy
        let add = simple 0x6Auy
        let sub = simple 0x6Buy
        let mul = simple 0x6Cuy
        let div_s = simple 0x6Duy
        let div_u = simple 0x6Euy
        let rem_s = simple 0x6Fuy
        let rem_u = simple 0x70uy
        let ``and`` = simple 0x71uy
        let ``or`` = simple 0x72uy
        let xor = simple 0x73uy
        let shl = simple 0x74uy
        let shr_s = simple 0x75uy
        let shr_u = simple 0x76uy
        let rotl = simple 0x77uy
        let rotr = simple 0x78uy




    module Memory =
        let private zero1 = ImmutableArray.Create<byte> 0uy
        let size = { Opcode = 0x3Fuy; Arguments = InstructionArguments.Bytes zero1 }
        let grow = { Opcode = 0x40uy; Arguments = InstructionArguments.Bytes zero1 }
        //let init


[<Interface>]
type IndexedVector<'IndexClass, 'T when 'IndexClass :> IndexKinds.Kind> =
    abstract Length: int32
    abstract First: Index<'IndexClass>
    abstract Item: index: Index<'IndexClass> -> inref<'T> with get

module IndexedVector =
    let ofBlockBuilder<'IndexClass, 'T when 'IndexClass :> IndexKinds.Kind> start (builder: ImmutableArray<'T>.Builder) =
        { new IndexedVector<'IndexClass, 'T> with
            member _.Length = builder.Count
            member _.First = start
            member this.Item with get index = &builder.ItemRef(int32 this.First + int32 index) }

    let ofBlock<'IndexClass, 'T when 'IndexClass :> IndexKinds.Kind> start (vector: ImmutableArray<'T>) =
        { new IndexedVector<'IndexClass, 'T> with
            member _.Length = vector.Length
            member _.First = start
            member this.Item with get index = &vector.ItemRef(int32 this.First + int32 index) }

type Expr = seq<InstructionSet.Instruction>

type TypeSection = IndexedVector<IndexKinds.Type, FuncType>

[<RequireQualifiedAccess>]
type ImportDesc =
    | Func of Index<IndexKinds.Type>
    | Table of TableType
    | Mem of MemType
    | Global of GlobalType

[<Struct>]
type Import<'Desc> = { Module: Name; Name: Name; Description: 'Desc }

let inline addImportDesc (builder: ImmutableArray<_>.Builder) (import: inref<Import<_>>) desc =
    builder.Add { Module = import.Module; Name = import.Name; Description = desc }

[<Sealed>]
type ImportSection (imports: ImmutableArray<Import<ImportDesc>>) =
    let functions = ImmutableArray.CreateBuilder()
    let tables = ImmutableArray.CreateBuilder()
    let memories = ImmutableArray.CreateBuilder()
    let globals = ImmutableArray.CreateBuilder()

    do
        for i = 0 to imports.Length - 1 do
            let import = &imports.ItemRef i
            match import.Description with
            | ImportDesc.Func i -> addImportDesc functions &import i
            | ImportDesc.Table t -> addImportDesc tables &import t
            | ImportDesc.Mem m -> addImportDesc memories &import m
            | ImportDesc.Global g -> addImportDesc globals &import g

    member _.Count = imports.Length
    member _.Item with get index = &imports.ItemRef index
    member val Functions = IndexedVector.ofBlockBuilder<IndexKinds.Func, _> Index.Zero functions
    member val Tables = IndexedVector.ofBlockBuilder<IndexKinds.Table, _> Index.Zero tables
    member val Memories = IndexedVector.ofBlockBuilder<IndexKinds.Mem, _> Index.Zero memories
    member val Globals = IndexedVector.ofBlockBuilder<IndexKinds.Global, _> Index.Zero globals

[<Struct; RequireQualifiedAccess>]
type Function = { Type: Index<IndexKinds.Type> }

type FunctionSection = IndexedVector<IndexKinds.Func, Function>

[<Struct; RequireQualifiedAccess>]
type Table = { Type: TableType }

type TableSection = IndexedVector<IndexKinds.Table, Table>

[<Struct; RequireQualifiedAccess>]
type Mem = { Type: MemType }

type MemorySection = IndexedVector<IndexKinds.Mem, Mem>

[<Struct>]
type Global = { Type: GlobalType; Expression: Expr }

type GlobalSection = IndexedVector<IndexKinds.Global, Global>

[<RequireQualifiedAccess>]
type ExportDesc =
    | Func of Index<IndexKinds.Func>
    | Table of Index<IndexKinds.Table>
    | Mem of Index<IndexKinds.Mem>
    | Global of Index<IndexKinds.Global>

[<Struct>]
type Export = { Name: Name; Description: ExportDesc }

type ExportSection = ImmutableArray<Export>

type StartSection = Index<IndexKinds.Func>

type Elem = | TODOFigureOutHowToRepresentElem

type ElementSection = IndexedVector<IndexKinds.Elem, Elem>

[<Struct>]
type Code = { Locals: ReadOnlyMemory<ValType>; Body: Expr }

type CodeSection = ImmutableArray<Code>

[<RequireQualifiedAccess>]
type Data =
    | Active of Index<IndexKinds.Mem> * Expr * ReadOnlyMemory<byte>
    | Passive of ReadOnlyMemory<byte>

type DataSection = IndexedVector<IndexKinds.Data, Data>

type DataCountSection = uint32

type Section =
    | CustomSection of CustomSection
    | TypeSection of TypeSection
    | ImportSection of ImportSection
    | FunctionSection of FunctionSection
    | TableSection of TableSection
    | MemorySection of MemorySection
    | GlobalSection of GlobalSection
    | ExportSection of ExportSection
    | StartSection of StartSection
    | ElementSection of ElementSection
    | CodeSection of CodeSection
    | DataSection of DataSection
    | DataCountSection of DataCountSection

module Section =
    let id section =
        match section with
        | CustomSection _ -> SectionId.Custom
        | TypeSection _ -> SectionId.Type
        | ImportSection _ -> SectionId.Import
        | FunctionSection _ -> SectionId.Function
        | TableSection _ -> SectionId.Table
        | MemorySection _ -> SectionId.Memory
        | GlobalSection _ -> SectionId.Global
        | ExportSection _ -> SectionId.Export
        | StartSection _ -> SectionId.Start
        | ElementSection _ -> SectionId.Element
        | CodeSection _ -> SectionId.Code
        | DataSection _ -> SectionId.Data
        | DataCountSection _ -> SectionId.DataCount

exception IncorrectSectionPositionException of section: Section
with
    override this.Message =
        let id = Section.id this.section
        sprintf
            "The section with ID %A (%i) is in the incorrect position, non-custom sections are required to be in ascending ID order"
            id
            (uint8 id)

exception DuplicateSectionException of existing: Section
with
    override this.Message =
        let id = Section.id this.existing
        sprintf "A non-custom section with the same ID %A (%i) already exists" id (uint8 id)

[<Struct>]
type ModuleSectionsEnumerator =
    val mutable private index: int32
    val private sections: ImmutableArray<Section>

    new (sections) = { sections = sections; index = -1 }

    member this.Current = this.sections.[this.index]

    member this.MoveNext() =
        if this.index < this.sections.Length then
            this.index <- this.index + 1
            this.index < this.sections.Length
        else
            false

    interface IEnumerator<Section> with
        member this.Current = this.Current :> obj
        member this.Current = this.Current
        member this.Reset() = this.index <- -1
        member this.MoveNext() = this.MoveNext()
        member _.Dispose() = ()

[<Struct>]
type ModuleSections (sections: ImmutableArray<Section>) =
    member _.Count = sections.Length

    member _.Item with get index = sections.[index]

    member _.GetEnumerator() = new ModuleSectionsEnumerator(sections)

    interface IReadOnlyList<Section> with
        member this.Count = this.Count
        member this.Item with get index = this.[index]
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator<Section>
        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator

module ModuleSections =
    [<RequireQualifiedAccess>]
    type private SectionsComparer () =
        interface IEqualityComparer<Section> with
            member _.Equals(x, y) =
                match x, y with
                | CustomSection _, _
                | _, CustomSection _ -> false
                | _, _ -> Section.id x = Section.id y

            member _.GetHashCode section = int32(Section.id section)

    let private comparer = SectionsComparer()

    [<Sealed>]
    type Builder (capacity) =
        let sections = ImmutableArray.CreateBuilder capacity
        let mutable next = SectionId.Custom

        new () = Builder 12

        member _.Count = sections.Count

        member _.TryAdd(section, duplicate: outref<Section>) =
            let id = Section.id section
            if id <> SectionId.Custom && id < next then raise(IncorrectSectionPositionException section)
            if id >= next then next <- id

            sections.Add section

            failwith "TODO: Create lookup"
            false

        member this.Add(section) =
            let (duplicate, _) = this.TryAdd section
            if duplicate then raise(DuplicateSectionException section)

        member _.ToImmutable() = ModuleSections(sections.ToImmutable())

    let tryOfSeq (sections: seq<_>) =
        use enumerator = sections.GetEnumerator()
        let builder = Builder()
        let mutable duplicate = ValueNone

        while duplicate.IsNone && enumerator.MoveNext() do
            let added, duplicate' = builder.TryAdd enumerator.Current
            if not added then duplicate <- ValueSome duplicate'

        match duplicate with
        | ValueNone -> Ok(builder.ToImmutable())
        | ValueSome duplicate' -> Error duplicate'

    let ofSeq sections =
        match tryOfSeq sections with
        | Ok sections' -> sections'
        | Error duplicate -> raise(DuplicateSectionException duplicate)

type WasmModule =
    { Version: uint32; Sections: ModuleSections }

[<Struct>]
type ValidatedModule = Validated of WasmModule

let (|ValidatedModule|) (Validated m) = m
