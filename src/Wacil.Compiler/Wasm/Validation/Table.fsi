namespace Wacil.Compiler.Wasm.Validation.Table

open System.Collections.Immutable
open System.Collections.Generic

open Wacil.Compiler.Wasm

type Index = int

[<RequireQualifiedAccess>]
type FunctionImport = { Name: string; Type: Format.FuncType }

[<RequireQualifiedAccess>]
type TableImport = { Name: string; Type: Format.TableType }

[<RequireQualifiedAccess>]
type MemoryImport = { Name: string; Limits: Format.Limits }

[<RequireQualifiedAccess>]
type GlobalImport = { Name: string; Type: Format.GlobalType }

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type ModuleImports =
    { Functions: ImmutableArray<FunctionImport>
      Tables: ImmutableArray<TableImport>
      Memories: ImmutableArray<MemoryImport>
      Globals: ImmutableArray<GlobalImport> }
      
[<Sealed>]
type ModuleImportLookup =
    internal new:
        lookup: Dictionary<string, ModuleImports> *
        functions: ImmutableArray<struct(string * FunctionImport)> -> ModuleImportLookup

    member Item: moduleImportName: string -> ModuleImports with get
    member Functions: ImmutableArray<struct(string * FunctionImport)>

    member Count: int

    interface IReadOnlyDictionary<string, ModuleImports>

[<NoComparison; StructuralEquality>]
type OperandType =
    | ValType of Format.ValType
    | UnknownType

[<AutoOpen>]
module OperandType =
    val (|IsNumType|): OperandType -> bool
    val (|IsVecType|): OperandType -> bool
    val (|IsRefType|): OperandType -> bool

[<NoComparison; StructuralEquality>]
type ValidInstruction =
    { Instruction: Format.Instruction
      PoppedTypes: ImmutableArray<OperandType>
      PushedTypes: ImmutableArray<OperandType>
      Unreachable: bool }

[<Sealed>]
type ValidExpression =
    internal new: source: ImmutableArray<Format.Instruction> -> ValidExpression

    member internal SetInstructions: ImmutableArray<ValidInstruction> -> unit

    member Source: ImmutableArray<Format.Instruction>
    member Instructions: ImmutableArray<ValidInstruction>
    //member BranchTargets: ImmutableArray<> // TODO: May not be necessary. Perhaps the labels can be kept track of during translation?

[<NoComparison; StructuralEquality>]
type Function =
    { Type: Format.FuncType
      LocalTypes: ImmutableArray<Format.ValType>
      Body: ValidExpression }

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type Global =
    { Type: Format.GlobalType
      Value: ValidExpression }

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ModuleExport =
    | Function of Function
    | Table
    | Memory of Index
    | Global

[<Sealed>]
type ModuleExportLookup =
    internal new:
        memories: Dictionary<Index, string> *
        functions: Dictionary<Index, string> *
        tables: Dictionary<Index, string> *
        lookup: Dictionary<string, ModuleExport> -> ModuleExportLookup

    member GetMemoryName: index: Index * name: outref<string> -> bool
    member GetFunctionName: index: Index * name: outref<string> -> bool
    member GetTableName: index: Index * name: outref<string> -> bool
    /// <summary>Gets an export corresponding to the specified <paramref name="name"/>.</summary>
    member Item: name: string -> ModuleExport with get

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ValidActiveData =
    { Memory: Index
      /// An expression that evaluates to an offset that the data is copied to.
      Offset: ValidExpression }

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ValidData =
    { /// The content of the data segment.
      Bytes: ImmutableArray<byte>
      Mode: ValidActiveData voption }
