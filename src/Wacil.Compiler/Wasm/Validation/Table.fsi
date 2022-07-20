namespace Wacil.Compiler.Wasm.Validation.Table

open System.Collections.Immutable
open System.Collections.Generic

open Wacil.Compiler.Wasm

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

[<RequireQualifiedAccess>]
module OperandType =
    val isNumType: OperandType -> bool
    val isVecType: OperandType -> bool
    val isRefType: OperandType -> bool

    val i32: OperandType
    val i64: OperandType
    val f32: OperandType
    val f64: OperandType
    val funcref: OperandType
    val externref: OperandType

    val fromRefType: Format.RefType -> OperandType

[<NoComparison; StructuralEquality>]
type ValidInstruction = { Instruction: Format.Instruction; Unreachable: bool }

[<Sealed>]
type ValidExpression =
    internal new:
        source: ImmutableArray<Format.Instruction> *
        parameterTypes: ImmutableArray<Format.ValType> *
        localTypes: ImmutableArray<Format.ValType> *
        resultTypes: ImmutableArray<Format.ValType> -> ValidExpression

    member internal SetInstructions: ImmutableArray<ValidInstruction> -> unit
    member internal SetMaximumIntroducedBlockCount: int -> unit

    member Source: ImmutableArray<Format.Instruction>
    /// <summary>The WebAssembly instructions of the expression, including the terminating <c>end</c> instruction.</summary>
    member Instructions: ImmutableArray<ValidInstruction>
    member ParameterTypes: ImmutableArray<Format.ValType>
    /// The types of the local variables used in the expression.
    member LocalTypes: ImmutableArray<Format.ValType>
    member ResultTypes: ImmutableArray<Format.ValType>
    /// The maximum number of blocks that are ever introduced in the expression.
    member MaximumIntroducedBlockCount: int

    member TryGetLocal: index: Format.LocalIdx * variableType: outref<Format.ValType> -> bool

[<NoComparison; StructuralEquality>]
type Function = { Type: Format.FuncType; Body: ValidExpression }

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type Global =
    { Type: Format.GlobalType
      Value: ValidExpression }

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ModuleExport =
    | Function of Function
    | Table
    | Memory of Format.MemIdx
    | Global

[<Sealed>]
type ModuleExportLookup =
    internal new:
        memories: Dictionary<Format.MemIdx, string> *
        functions: Dictionary<Format.FuncIdx, string> *
        tables: Dictionary<Format.TableIdx, string> *
        lookup: Dictionary<string, ModuleExport> -> ModuleExportLookup

    member GetMemoryName: index: Format.MemIdx * name: outref<string> -> bool
    member GetFunctionName: index: Format.FuncIdx * name: outref<string> -> bool
    member GetTableName: index: Format.TableIdx * name: outref<string> -> bool
    /// <summary>Gets an export corresponding to the specified <paramref name="name"/>.</summary>
    member Item: name: string -> ModuleExport with get

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ValidActiveData =
    { Memory: Format.MemIdx
      /// An expression that evaluates to an offset that the data is copied to.
      Offset: ValidExpression }

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ValidData =
    { /// The content of the data segment.
      Bytes: ImmutableArray<byte>
      Mode: ValidActiveData voption }
