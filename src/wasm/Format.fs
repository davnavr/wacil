module Wasm.Format

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Immutable

let preamble = ImmutableArray.Create<byte> "\0asm"B
let [<Literal>] CurrentVersion = 1u
let [<Literal>] PageSize = 65536u

[<Struct>]
type MemSize (multiple: uint16) =
    member _.Size = uint32 multiple * uint32 PageSize

    static member inline op_Implicit(size: MemSize) = size.Size
    static member inline op_Explicit(size: MemSize) = Checked.int32 size.Size

module MemSize =
    let ofMultiple multiple = MemSize multiple

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

[<Struct>]
type Index<'Class> =
    val Index: uint32

    new (index) = { Index = index }

    static member inline op_Implicit(index: Index<'Class>) = index.Index
    static member inline op_Explicit(index: Index<'Class>) = Checked.int32 index.Index

let inline (|Index|) (index: Index<_>) = index.Index

[<Interface>]
type IndexedVector<'IndexClass, 'T> =
    abstract Length: int32
    abstract First: Index<'IndexClass>
    abstract Item: index: Index<'IndexClass> -> inref<'T>

[<Struct>]
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

type Limit<'T when 'T : struct and 'T :> IEquatable<'T>> = { Min: 'T; Max: 'T voption }

[<Struct>]
type TableType = { ElementType: RefType; Limit: Limit<uint32> }

type MemType = Limit<MemSize>

type Expr = unit

type GlobalType =
    | Const of ValType
    | Var of ValType

type TypeSection = IndexedVector<FuncType, FuncType>

[<RequireQualifiedAccess>]
type ImportDesc =
    | Func of Index<FuncType>
    | Table of TableType
    | Mem of MemType
    | Global of GlobalType

[<Struct>]
type Import = { Module: Name; Name: Name; Description: ImportDesc }

type ImportSection = IndexedVector<Import, Import>

[<Struct; RequireQualifiedAccess>]
type Function = { Type: FuncType }

type FunctionSection = IndexedVector<Function, Function>

[<Struct; RequireQualifiedAccess>]
type Table = { Type: TableType }

type TableSection = IndexedVector<Table, Table>

[<Struct; RequireQualifiedAccess>]
type Mem = { Type: MemType }

type MemorySection = IndexedVector<Mem, Mem>

[<Struct>]
type Global = { Type: GlobalType; Expression: Expr }

type GlobalSection = IndexedVector<Global, Global>

[<RequireQualifiedAccess>]
type ExportDesc =
    | Func of Index<Function>
    | Table of Index<Table>
    | Mem of Index<Mem>
    | Global of Index<Global>

[<Struct>]
type Export = { Name: Name; Description: ExportDesc }

type ExportSection = IndexedVector<Export, Export>

type StartSection = Index<Function>

type Elem = | TODOFigureOutHowToRepresentElem

[<RequireQualifiedAccess>]
type ElementSection = | TODO

[<Struct>]
type Code = { Locals: ReadOnlyMemory<ValType>; Body: Expr }

type CodeSection = IndexedVector<Code, Code>

[<RequireQualifiedAccess>]
type Data =
    | Active of Index<Mem> * Expr * ReadOnlyMemory<byte>
    | Passive of ReadOnlyMemory<byte>

type DataSection = IndexedVector<Data, Data>

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

exception DuplicateSectionException of existing: Section
with
    override this.Message =
        let id = Section.id this.existing
        sprintf "A non-custom section with the same id %A (%i) already exists" id (uint8 id)

[<Struct>]
type ModuleSectionsEnumerator =
    val mutable private index: int32
    val private sections: ImmutableArray<Section>

    new (sections) = { sections = sections; index = -1 }

    member this.Current = this.sections.[this.index]

    member this.MoveNext() =
        if this.index < this.sections.Length then
            this.index <- this.index + 1
            true
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

        new () = Builder 12

        member _.TryAdd(section, duplicate: outref<Section>) =
            sections.Add section
            failwith "TODO: Create lookup"
            false

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
