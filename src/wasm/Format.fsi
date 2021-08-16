module Wasm.Format

open System
open System.Collections
open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

[<Literal>] val PageSize: uint32 = 65536u

[<RequireQualifiedAccess>]
module Preamble =
    val magic : ImmutableArray<byte>
    val version : ImmutableArray<byte>

type Name = string

/// Represents the size of a memory, which is always a multiple of the page size.
[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type MemSize =
    val Multiple: uint16
    member Size: uint32

    internal new: multiple: uint16 -> MemSize

    override ToString: unit -> string

    static member inline op_Implicit: size: MemSize -> uint32
    static member inline op_Explicit: size: MemSize -> int32

    interface IEquatable<MemSize>

module Types =
    type WasmType =
        | externref = 0x6Fuy
        | funcref = 0x70uy
        | i32 = 0x7Fuy
        | i64 = 0x7Euy
        | f32 = 0x7Duy
        | f64 = 0x7Cuy

    [<NoComparison; StructuralEquality>]
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

    [<Sealed>]
    type Limit<'T when 'T : struct and 'T : equality> =
        member Min: 'T
        member Max: 'T voption

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

    [<RequireQualifiedAccess>]
    module Limit =
        val ofMin: min: 'T -> Limit<'T>
        val tryWithMax: min: 'T -> max: 'T voption -> Limit<'T> voption when 'T : comparison

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

[<RequireQualifiedAccess>]
module IndexKinds =
    [<AbstractClass>]
    type Kind = class end

    [<AbstractClass; Sealed>]
    type Type = inherit Kind

    [<AbstractClass; Sealed>]
    type Func = inherit Kind

    [<AbstractClass; Sealed>]
    type Table = inherit Kind

    [<AbstractClass; Sealed>]
    type Mem = inherit Kind

    [<AbstractClass; Sealed>]
    type Global = inherit Kind

    [<AbstractClass; Sealed>]
    type Elem = inherit Kind

    [<AbstractClass; Sealed>]
    type Data = inherit Kind

    [<AbstractClass; Sealed>]
    type Local = inherit Kind

    [<AbstractClass; Sealed>]
    type Label = inherit Kind

[<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
type Index<'Class when 'Class :> IndexKinds.Kind> =
    val Index: uint32

    new: index: uint32 -> Index<'Class>

    override ToString: unit -> string

    static member inline Zero: Index<'Class>
    static member (+): index: Index<'Class> * offset: uint32 -> Index<'Class>
    static member (-): x: Index<'Class> * y: Index<'Class> -> uint32
    static member inline op_Implicit: index: Index<'Class> -> uint32
    static member inline op_Explicit: index: Index<'Class> -> int32

[<Interface>]
type IndexedVector<'IndexClass, 'T when 'IndexClass :> IndexKinds.Kind> =
    abstract Length: int32
    abstract First: Index<'IndexClass>
    abstract Item: index: Index<'IndexClass> -> inref<'T> with get

[<RequireQualifiedAccess>]
module internal IndexedVector =
    val ofBlockBuilder<'IndexClass, 'T when 'IndexClass :> IndexKinds.Kind> :
        start: Index<'IndexClass> ->
        builder: ImmutableArray<'T>.Builder ->
        IndexedVector<'IndexClass, 'T>

    val ofBlock<'IndexClass, 'T when 'IndexClass :> IndexKinds.Kind> :
        start: Index<'IndexClass> ->
        vector: ImmutableArray<'T> ->
        IndexedVector<'IndexClass, 'T>

val inline (|Index|) : index: Index<'Class> -> uint32

open Types

module InstructionSet =
    type Opcode = uint8

    //type Opcode = { Name: string; Opcode: uint8; Arguments: ? }

    // and/or

    // (|unreachable|): Opcode -> bool
    // (|nop|): Opcode -> bool

    (*
    type InstructionSomeThing =
        | Unreachable
        | Nop
        | Control of ?
        | I32 of ?
    *)

    [<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
    type MemArgAlignment =
        member Alignment: uint32

        static member inline op_Implicit: alignment: MemArgAlignment -> uint32

        interface IEquatable<MemArgAlignment>

    val inline (|MemArgAlignment|) : alignment: MemArgAlignment -> uint32

    [<RequireQualifiedAccess>]
    module MemArgAlignment =
        val ofPower : power: uint8 -> MemArgAlignment

    [<IsReadOnly; Struct; StructuralComparison; StructuralEquality>]
    type MemArg =
        { Alignment: MemArgAlignment
          Offset: uint32 }

    [<RequireQualifiedAccess; NoComparison; NoEquality>]
    type BlockType =
        | Empty
        | ValueType of ValType
        | Index of Index<IndexKinds.Type>

    [<RequireQualifiedAccess; NoComparison; NoEquality>]
    type InstructionArguments =
        | Nothing
        | Bytes of ImmutableArray<byte>
        | RefType of RefType
        //| Block of BlockType * Expr
        //| BlockElse of BlockType * seq<Instruction> * Expr
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

    [<IsReadOnly; Struct; NoComparison; NoEquality>]
    type Instruction =
        { Opcode: Opcode
          Arguments: InstructionArguments }

    [<AutoOpen>]
    module Control =
        val ``unreachable`` : Instruction
        val ``nop`` : Instruction
        val ``block`` : BlockType -> Instruction
        val ``loop`` : BlockType -> Instruction
        val ``if`` : BlockType -> Instruction
        val ``else`` : Instruction

        val ``end`` : Instruction
        val ``br`` : label: Index<IndexKinds.Label> -> Instruction
        val br_if : label: Index<IndexKinds.Label> -> Instruction
        val br_table : labels: ImmutableArray<Index<IndexKinds.Label>> -> Instruction
        val ``return`` : Instruction
        val call : Index<IndexKinds.Func> -> Instruction
        val call_indirect : Index<IndexKinds.Type> -> Index<IndexKinds.Table> -> Instruction

    [<RequireQualifiedAccess>]
    module Ref =
        val ``null`` : t: RefType -> Instruction
        val is_null : Instruction
        val ``func`` : Index<IndexKinds.Func> -> Instruction

    module Parametric =
        val drop : Instruction
        val select : types: ImmutableArray<ValType> voption -> Instruction

    [<RequireQualifiedAccess>]
    module Local =
        val ``get`` : variable: Index<IndexKinds.Local> -> Instruction
        val ``set`` : variable: Index<IndexKinds.Local>  -> Instruction
        val tee : variable: Index<IndexKinds.Local>  -> Instruction

    [<RequireQualifiedAccess>]
    module Global =
        val ``get`` : variable: Index<IndexKinds.Global> -> Instruction
        val ``set`` : variable: Index<IndexKinds.Global> -> Instruction

    [<RequireQualifiedAccess>]
    module Table =
        val ``get`` : table: Index<IndexKinds.Table> -> Instruction
        val ``set`` : table: Index<IndexKinds.Table> -> Instruction

        //val init : source: Index<IndexKinds.Elem> -> destination: Index<IndexKinds.Table> -> Instruction
        //val copy : destination: Index<IndexKinds.Table> -> source: Index<IndexKinds.Table> -> Instruction
        //val grow : table: Index<IndexKinds.Table> -> Instruction
        //val size : table: Index<IndexKinds.Table> -> Instruction
        //val fill : table: Index<IndexKinds.Table> -> Instruction

    //[<RequireQualifiedAccess>]
    //module Elem =
    //    val drop : index: Index<IndexKinds.Elem> -> Instruction

    /// Contains memory instructions for storing and loading 32-bit integers.
    [<RequireQualifiedAccess>]
    module I32 =
        val load : m: MemArg -> Instruction

        val load8_s : m: MemArg -> Instruction
        val load8_u : m: MemArg -> Instruction
        val load16_s : m: MemArg -> Instruction
        val load16_u : m: MemArg -> Instruction

        val store : m: MemArg -> Instruction

        val store8 : m: MemArg -> Instruction
        val store16 : m: MemArg -> Instruction

        val ``const`` : n: int32 -> Instruction

        val eqz : Instruction
        val eq : Instruction
        val ne : Instruction
        val lt_s : Instruction
        val lt_u : Instruction
        val gt_s : Instruction
        val gt_u : Instruction
        val le_s : Instruction
        val le_u : Instruction
        val ge_s : Instruction
        val ge_u : Instruction

        val clz : Instruction
        val ctz : Instruction
        val popcnt : Instruction
        val add : Instruction
        val sub : Instruction
        val mul : Instruction
        val div_s : Instruction
        val div_u : Instruction
        val rem_s : Instruction
        val rem_u : Instruction
        val ``and`` : Instruction
        val ``or`` : Instruction
        val xor : Instruction
        val shl : Instruction
        val shr_s : Instruction
        val shr_u : Instruction
        val rotl : Instruction
        val rotr : Instruction

        //val wrap_i64 : Instruction
        //val trunc_f32_s : Instruction
        //val trunc_f32_u : Instruction
        //val trunc_f64_s : Instruction
        //val trunc_f64_u : Instruction

        //val reinterpret_f32 : Instruction

        //val extend8_s : Instruction
        //val extend16_s : Instruction

        //val trunc_sat_f32_s : Instruction
        //val trunc_sat_f32_u : Instruction
        //val trunc_sat_f64_s : Instruction
        //val trunc_sat_f64_u : Instruction

    (*
    /// Contains memory instructions for storing and loading 64-bit integers.
    [<RequireQualifiedAccess>]
    module I64 =
        val load : MemArg -> Instruction

        val load8_s : m: MemArg -> Instruction
        val load8_u : m: MemArg -> Instruction
        val load16_s : m: MemArg -> Instruction
        val load16_u : m: MemArg -> Instruction
        val load32_s : m: MemArg -> Instruction
        val load32_u : m: MemArg -> Instruction

        val store : m: MemArg -> Instruction

        val store8 : m: MemArg -> Instruction
        val store16 : m: MemArg -> Instruction
        val store32 : m: MemArg -> Instruction

        val ``const`` : n: int64 -> Instruction

        val eqz : Instruction
        val eq : Instruction
        val ne : Instruction
        val lt_s : Instruction
        val lt_u : Instruction
        val gt_s : Instruction
        val gt_u : Instruction
        val le_s : Instruction
        val le_u : Instruction
        val ge_s : Instruction
        val ge_u : Instruction

        val clz : Instruction
        val ctz : Instruction
        val popcnt : Instruction
        val add : Instruction
        val sub : Instruction
        val mul : Instruction
        val div_s : Instruction
        val div_u : Instruction
        val rem_s : Instruction
        val rem_u : Instruction
        val ``and`` : Instruction
        val ``or`` : Instruction
        val xor : Instruction
        val shl : Instruction
        val shr_s : Instruction
        val shr_u : Instruction
        val rotl : Instruction
        val rotr : Instruction

        val extend_i32_s : Instruction
        val extend_i32_u : Instruction
        val trunc_f32_s : Instruction
        val trunc_f32_u : Instruction
        val trunc_f64_s : Instruction
        val trunc_f64_u : Instruction

        val reinterpret_f64 : Instruction

        val extend8_s : Instruction
        val extend16_s : Instruction
        val extend32_s : Instruction

        val trunc_sat_f32_s : Instruction
        val trunc_sat_f32_u : Instruction
        val trunc_sat_f64_s : Instruction
        val trunc_sat_f64_u : Instruction

    /// Contains memory instructions for storing and loading 32-bit floating-point numbers.
    [<RequireQualifiedAccess>]
    module F32 =
        val load : MemArg -> Instruction

        val store : m: MemArg -> Instruction

        val ``const`` : n: Single -> Instruction

        val eq : Instruction
        val ne : Instruction
        val lt : Instruction
        val gt : Instruction
        val le : Instruction
        val ge : Instruction

        val abs : Instruction
        val neg : Instruction
        val ceil : Instruction
        val floor : Instruction
        val trunc : Instruction
        val nearest : Instruction
        val sqrt : Instruction
        val add : Instruction
        val sub : Instruction
        val mul : Instruction
        val div : Instruction
        val min : Instruction
        val max : Instruction
        val copysign : Instruction

        val convert_i32_s : Instruction
        val convert_i32_u : Instruction
        val convert_i64_s : Instruction
        val convert_i64_u : Instruction
        val demote_f64 : Instruction

        val reinterpret_i32 : Instruction

    /// Contains memory instructions for storing and loading 64-bit floating-point numbers.
    [<RequireQualifiedAccess>]
    module F64 =
        val load : MemArg -> Instruction

        val store : m: MemArg -> Instruction

        val ``const`` : n: Double -> Instruction

        val eq : Instruction
        val ne : Instruction
        val lt : Instruction
        val gt : Instruction
        val le : Instruction
        val ge : Instruction

        val abs : Instruction
        val neg : Instruction
        val ceil : Instruction
        val floor : Instruction
        val trunc : Instruction
        val nearest : Instruction
        val sqrt : Instruction
        val add : Instruction
        val sub : Instruction
        val mul : Instruction
        val div : Instruction
        val min : Instruction
        val max : Instruction
        val copysign : Instruction

        val convert_i32_s : Instruction
        val convert_i32_u : Instruction
        val convert_i64_s : Instruction
        val convert_i64_u : Instruction
        val promote_f32 : Instruction

        val reinterpret_i64 : Instruction
    *)

    [<RequireQualifiedAccess>]
    module Memory =
        val size : Instruction
        val grow : Instruction

        //val init : Index<IndexKinds.Data> -> Instruction
        //val copy : Instruction
        //val fill : Instruction

    //[<RequireQualifiedAccess>]
    //module Data =
    //    val drop : Index<IndexKinds.Data> -> Instruction

/// Represents a function body or the value of a global variable.
[<IsReadOnly; Struct; RequireQualifiedAccess; NoComparison; NoEquality>]
type Expr =
    internal
        { Instructions: ImmutableArray<InstructionSet.Instruction> }

[<RequireQualifiedAccess>]
module Expr =
    val toBlock : expression: Expr -> ImmutableArray<InstructionSet.Instruction>

/// (1) Defines the function types used by the module.
type TypeSection = IndexedVector<IndexKinds.Type, FuncType>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ImportDesc =
    | Func of Index<IndexKinds.Type>
    | Table of TableType
    | Mem of MemType
    | Global of GlobalType

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type Import<'Desc> =
    { Module: Name
      Name: Name
      Description: 'Desc }

/// (2) Defines the functions, tables, memories, and globals defined in another module used by the module.
[<Sealed>]
type ImportSection =
    new: imports: ImmutableArray<Import<ImportDesc>> -> ImportSection

    member Count : int32
    member Item: index: int32 -> inref<Import<ImportDesc>> with get

    member Functions: IndexedVector<IndexKinds.Func, Import<Index<IndexKinds.Type>>>
    member Tables: IndexedVector<IndexKinds.Table, Import<TableType>>
    member Memories: IndexedVector<IndexKinds.Mem, Import<MemType>>
    member Globals: IndexedVector<IndexKinds.Global, Import<GlobalType>>

[<IsReadOnly; Struct; RequireQualifiedAccess; NoComparison; NoEquality>]
type Function =
    { Type: Index<IndexKinds.Type> }

/// (3) Specifies the signatures of the functions defined in the module.
type FunctionSection = IndexedVector<IndexKinds.Func, Function>

[<IsReadOnly; Struct; RequireQualifiedAccess; NoComparison; NoEquality>]
type Table =
    { Type: TableType }

/// (4)
type TableSection = IndexedVector<IndexKinds.Table, Table>

[<IsReadOnly; Struct; RequireQualifiedAccess; NoComparison; NoEquality>]
type Mem =
    { Type: MemType }

/// (5)
type MemorySection = IndexedVector<IndexKinds.Mem, Mem>

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type Global =
    { Type: GlobalType
      Expression: Expr }

/// (6) Defines the global variables of a module.
type GlobalSection = IndexedVector<IndexKinds.Global, Global>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ExportDesc =
    | Func of Index<IndexKinds.Func>
    | Table of Index<IndexKinds.Table>
    | Mem of Index<IndexKinds.Mem>
    | Global of Index<IndexKinds.Global>

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type Export =
    { Name: Name
      Description: ExportDesc }

/// (7) Specifies the functions, tables, memories, and globals in the module that are exported.
type ExportSection = ImmutableArray<Export>

/// (8) Specifies the index of the function "that is automatically called invoked when the module is instantiated".
type StartSection = Index<IndexKinds.Func>

type Elem
//    | 

/// (9)
type ElementSection = IndexedVector<IndexKinds.Elem, Elem>

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type Locals =
    { Count: uint32
      Type: ValType }

/// Represents the body of a function.
[<IsReadOnly; Struct; NoComparison; NoEquality>]
type Code =
    { Locals: ImmutableArray<Locals>
      Body: Expr }

/// (10) Contains the local variables and bodies of the functions defined in the module.
type CodeSection = ImmutableArray<Code>

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type Data =
    | Active of Index<IndexKinds.Mem> * Expr * ReadOnlyMemory<byte>
    | Passive of ReadOnlyMemory<byte>

/// (11)
type DataSection = IndexedVector<IndexKinds.Data, Data>

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

exception IncorrectSectionPositionException of section: Section

type IncorrectSectionPositionException with override Message: string

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

        member Count: int32

        /// <exception cref="T:Wasm.Format.DuplicateSectionException" />
        /// <exception cref="T:Wasm.Format.IncorrectSectionPositionException" />
        member Add: section: Section -> unit
        // TODO: Consider throwing exception if length of code section vector <> length of function section vector.
        /// <exception cref="T:Wasm.Format.IncorrectSectionPositionException" />
        member TryAdd: section: Section -> bool
        member ToImmutable: unit -> ModuleSections

    val tryOfSeq : sections: seq<Section> -> Result<ModuleSections, Section>
    /// <exception cref="T:Wasm.Format.DuplicateSectionException" />
    val ofSeq : sections: seq<Section> -> ModuleSections

[<NoComparison; NoEquality>]
type KnownSections =
    { TypeSection: TypeSection voption
      ImportSection: ImportSection voption
      FunctionSection: FunctionSection voption
      TableSection: TableSection voption
      MemorySection: MemorySection voption
      GlobalSection: GlobalSection voption
      ExportSection: ExportSection voption
      StartSection: StartSection voption
      ElementSection: ElementSection voption
      CodeSection: CodeSection voption
      DataSection: DataSection voption
      DataCountSection: DataCountSection voption }

// TODO: Add a name custom section and allow retrieval of names from there.

[<NoComparison; NoEquality>]
type WasmModule =
    { Version: uint32
      Sections: ModuleSections }

val getKnownSections : WasmModule -> KnownSections

[<NoComparison; NoEquality>]
type ModuleExports

[<RequireQualifiedAccess>]
module ModuleExports =
    val tryGetFunction : exports: ModuleExports -> func: Index<IndexKinds.Func> -> Export voption
    val tryGetMemory : exports: ModuleExports -> mem: Index<IndexKinds.Mem> -> Export voption
    val memories : exports: ModuleExports -> seq<struct(Name * Index<IndexKinds.Mem>)>

val getModuleExports : ExportSection -> ModuleExports

[<IsReadOnly; Struct; NoComparison; NoEquality>]
type ValidatedModule = internal Validated of WasmModule

val (|ValidatedModule|) : ValidatedModule -> WasmModule
