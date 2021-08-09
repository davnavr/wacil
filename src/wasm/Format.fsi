module Wasm.Format

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

val preamble : ImmutableArray<byte>

[<Literal>] val CurrentVersion : uint32 = 1u
[<Literal>] val PageSize: uint32 = 65536u

type Name = string

/// Represents the size of a memory, which is always a multiple of the page size.
[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type MemSize =
    member Size: uint32

    interface IEquatable<MemSize>

[<NoComparison; NoEquality>]
type CustomSection =
    { Name: Name
      Contents: ImmutableArray<byte> }

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

[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type Index<'Class> =
    val Index: uint32

    new: index: uint32 -> Index<'Class>

    static member inline op_Implicit: index: Index<'Class> -> uint32
    static member inline op_Explicit: index: Index<'Class> -> int32

[<Interface>]
type IndexedVector<'IndexClass, 'T> =
    abstract Length: int32
    abstract First: Index<'IndexClass>
    abstract Item: index: Index<'IndexClass> -> inref<'T>

val inline (|Index|) : index: Index<'Class> -> uint32

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type NumType =
    | I32
    | I64
    | F32
    | F64

    interface IEquatable<NumType>

[<NoComparison; StructuralEquality>]
type RefType =
    | FuncRef
    | ExternRef

    interface IEquatable<RefType>

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type ValType =
    | NumType of numType: NumType
    | RefType of refType: RefType

    interface IEquatable<ValType>

/// Represents the types of the results of executing an instruction or calling a function.
type ResultType = ImmutableArray<ValType>

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type FuncType =
    { Parameters: ResultType
      Results: ResultType }

    interface IEquatable<FuncType>

[<NoComparison; StructuralEquality>]
type Limit<'T when 'T : struct and 'T :> IEquatable<'T>> =
    { Min: 'T
      Max: 'T voption }

    interface IEquatable<Limit<'T>>

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type TableType =
    { ElementType: RefType
      Limit: Limit<uint32> }

    interface IEquatable<TableType>

type MemType = Limit<MemSize>

type GlobalType =
    | Const of ValType
    | Var of ValType

//module InstructionSet

/// Represents a function body or the value of a global variable.
type Expr = unit

/// (1) Defines the function types used by the module.
type TypeSection = IndexedVector<FuncType, FuncType>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ImportDesc =
    | Func of Index<FuncType>
    | Table of TableType
    | Mem of MemType
    | Global of GlobalType

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type Import =
    { Module: Name
      Name: Name
      Description: ImportDesc }

/// (2) Defines the functions, tables, memories, and globals defined in another module used by the module.
type ImportSection = IndexedVector<Import, Import>

[<IsReadOnly; Struct; RequireQualifiedAccess; NoComparison; NoEquality>]
type Function =
    { Type: FuncType }

/// (3) Specifies the signatures of the functions defined in the module.
type FunctionSection = IndexedVector<Function, Function>

[<IsReadOnly; Struct; RequireQualifiedAccess; NoComparison; NoEquality>]
type Table =
    { Type: TableType }

/// (4)
type TableSection = IndexedVector<Table, Table>

[<IsReadOnly; Struct; RequireQualifiedAccess; NoComparison; NoEquality>]
type Mem =
    { Type: MemType }

/// (5)
type MemorySection = IndexedVector<Mem, Mem>

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type Global =
    { Type: GlobalType
      Expression: Expr }

/// (6) Defines the global variables of a module.
type GlobalSection = IndexedVector<Global, Global>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ExportDesc =
    | Func of Index<Function>
    | Table of Index<Table>
    | Mem of Index<Mem>
    | Global of Index<Global>

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type Export =
    { Name: Name
      Description: ExportDesc }

/// (7) Specifies the functions, tables, memories, and globals in the module that are exported.
type ExportSection = IndexedVector<Export, Export>

/// (8) Specifies the index of the function "that is automatically called invoked when the module is instantiated".
type StartSection = Index<Function>

type Elem
//    | 

/// (9)
type ElementSection //= IndexedVector<Elem, Elem>

/// Represents the body of a function.
[<IsReadOnly; Struct; NoComparison; NoEquality>]
type Code =
    { Locals: ReadOnlyMemory<ValType>
      Body: Expr }

/// (10) Contains the local variables and bodies of the functions defined in the module.
type CodeSection = IndexedVector<Code, Code>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type Data =
    | Active of Index<Mem> * Expr * ReadOnlyMemory<byte>
    | Passive of ReadOnlyMemory<byte>

/// (11)
type DataSection = IndexedVector<Data, Data>

/// (12) Optional number than indicates "the number of data segments in the data section".
type DataCountSection = uint32

[<NoComparison; NoEquality>]
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

[<Sealed>]
exception DuplicateSectionException of existing: Section

type DuplicateSectionException with override Message: string

type MemSize with
    static member inline op_Implicit: size: MemSize -> uint32
    static member inline op_Explicit: size: MemSize -> int32

[<RequireQualifiedAccess>]
module MemSize =
    val ofMultiple : multiple: uint16 -> MemSize

[<RequireQualifiedAccess>]
module Section =
    val id : section: Section -> SectionId

[<Struct; NoComparison; NoEquality>]
type ModuleSectionsEnumerator =
    val mutable private index: int32
    val private sections: ImmutableArray<Section>

    member Current: Section
    member MoveNext: unit -> bool

    interface IEnumerator<Section>
    interface IEnumerator
    interface IDisposable

/// Represents the sections of a module.
[<IsReadOnly; Struct; NoComparison; NoEquality>]
type ModuleSections =
    member Count: int32

    /// <exception cref="T:System.ArgumentOutOfRangeException">
    /// Thrown when the <paramref name="index"/> is negative or exceeds the index of the last section in the module.
    /// </exception>
    member Item: index: int32 -> Section with get

    member GetEnumerator: unit -> ModuleSectionsEnumerator

    interface IReadOnlyList<Section>
    interface IReadOnlyCollection<Section>
    interface IEnumerable<Section>
    interface IEnumerable

[<RequireQualifiedAccess>]
module ModuleSections =
    [<Sealed>]
    type Builder =
        new: capacity: int32 -> Builder
        new: unit -> Builder

        member TryAdd: section: Section * duplicate: outref<Section> -> bool
        member ToImmutable: unit -> ModuleSections

    val tryOfSeq : sections: seq<Section> -> Result<ModuleSections, Section>
    val ofSeq : sections: seq<Section> -> ModuleSections

[<NoComparison; NoEquality>]
type WasmModule =
    { Version: uint32
      Sections: ModuleSections }
