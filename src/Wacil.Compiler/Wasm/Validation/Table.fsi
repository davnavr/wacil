namespace Wacil.Compiler.Wasm.Validation.Table

open System.Collections.Immutable
open System.Collections.Generic

open Wacil.Compiler.Wasm

[<RequireQualifiedAccess>]
type FunctionImport = { Index: Format.FuncIdx; Name: string; Type: Format.FuncType }

[<RequireQualifiedAccess>]
type TableImport = { Index: Format.TableIdx; Name: string; Type: Format.TableType }

[<RequireQualifiedAccess>]
type MemoryImport = { Index: Format.MemIdx; Name: string; Limits: Format.Limits }

[<RequireQualifiedAccess>]
type GlobalImport = { Index: Format.GlobalIdx; Name: string; Type: Format.GlobalType }

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type ModuleImports =
    { Functions: ImmutableArray<FunctionImport>
      Tables: ImmutableArray<TableImport>
      Memories: ImmutableArray<MemoryImport>
      Globals: ImmutableArray<GlobalImport> }
      
[<Sealed>]
type ModuleImportLookup =
    internal new: lookup: SortedDictionary<string, ModuleImports> * imports: ModuleImports -> ModuleImportLookup

    /// <summary>Gets the imports corresponding to the specified module <paramref name="name"/>.</summary>
    member Item: name: string -> ModuleImports with get
    /// Gets the names of all imported modules, in a consistent order.
    member Modules: IReadOnlyCollection<string>
    member Imports: ModuleImports

[<NoComparison; StructuralEquality>]
type OperandType =
    | ValType of Format.ValType
    | UnknownType

[<RequireQualifiedAccess>]
module OperandType =
    val isNumType: OperandType -> bool
    val isVecType: OperandType -> bool
    val isRefType: OperandType -> bool

    val i32 : OperandType
    val i64 : OperandType
    val f32 : OperandType
    val f64 : OperandType
    val funcref : OperandType
    val externref : OperandType
    val v128 : OperandType

    val fromRefType: Format.RefType -> OperandType

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type StackState =
    | Omitted
    | PoppedValues of ImmutableArray<Format.ValType>

    /// <summary>
    /// Gets the types of the values that were popped from the stack, with the first item representing the value that was popped
    /// first.
    /// </summary>
    /// <exception cref="T:System.InvalidOperationException">Thrown when the stack state was omitted.</exception>
    member PoppedTypes : ImmutableArray<Format.ValType>

[<NoComparison; StructuralEquality>]
type ValidInstruction =
    { Instruction: Format.Instruction
      StackState: StackState
      Unreachable: bool }

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
type Global = { Type: Format.GlobalType; Value: ValidExpression }

[<RequireQualifiedAccess; NoComparison; ReferenceEquality>]
type AnyFunction =
    | Defined of int * Function
    | Import of int * FunctionImport

    member Type : Format.FuncType

[<RequireQualifiedAccess; NoComparison; ReferenceEquality>]
type AnyMemory =
    | Defined of int * Format.Limits
    | Import of int * MemoryImport

    member Limits : Format.Limits

[<RequireQualifiedAccess; NoComparison; ReferenceEquality>]
type AnyTable =
    | Defined of int * Format.TableType
    | Import of int * TableImport

    member Type : Format.TableType

[<RequireQualifiedAccess; NoComparison; ReferenceEquality>]
type AnyGlobal =
    | Defined of int * Global
    | Import of int * GlobalImport

    member Type : Format.GlobalType

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ModuleExport =
    | Function of Format.FuncIdx * AnyFunction
    | Table of Format.TableIdx * AnyTable
    | Memory of Format.MemIdx * AnyMemory
    | Global of Format.GlobalIdx * AnyGlobal

[<Sealed>]
type ModuleExportLookup =
    internal new:
        functions: Dictionary<Format.FuncIdx, string> *
        tables: Dictionary<Format.TableIdx, string> *
        memories: Dictionary<Format.MemIdx, string> *
        globals: Dictionary<Format.GlobalIdx, string> *
        lookup: Dictionary<string, ModuleExport> -> ModuleExportLookup

    member GetMemoryName: index: Format.MemIdx * name: outref<string> -> bool
    member GetFunctionName: index: Format.FuncIdx * name: outref<string> -> bool
    member GetTableName: index: Format.TableIdx * name: outref<string> -> bool
    member GetGlobalName: index: Format.GlobalIdx * name: outref<string> -> bool
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

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ValidElementMode =
    | Active of Format.TableIdx * offset: ValidExpression
    | Passive
    | Declarative

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ValidElement =
    { Type: Format.RefType
      Elements: ImmutableArray<ValidExpression>
      Mode: ValidElementMode }
