namespace Wacil.Compiler.Wasm.Validation.Table

open System.Collections.Immutable
open System.Collections.Generic

open Wacil.Compiler.Wasm.Format

[<RequireQualifiedAccess>]
type FunctionImport = { Index: FuncIdx; Name: string; Type: FuncType }

[<RequireQualifiedAccess>]
type TableImport = { Index: TableIdx; Name: string; Type: TableType }

[<RequireQualifiedAccess>]
type MemoryImport = { Index: MemIdx; Name: string; Limits: Limits }

[<RequireQualifiedAccess>]
type GlobalImport = { Index: GlobalIdx; Name: string; Type: GlobalType }

[<RequireQualifiedAccess>]
type ModuleImports =
    { Functions: ImmutableArray<FunctionImport>
      Tables: ImmutableArray<TableImport>
      Memories: ImmutableArray<MemoryImport>
      Globals: ImmutableArray<GlobalImport> }

[<Sealed>]
type ModuleImportLookup internal (lookup: SortedDictionary<string, ModuleImports>, imports: ModuleImports) =
    member _.Item with get name =
        if isNull lookup then
            raise(KeyNotFoundException "modules does not contain any imports")
        lookup[name]

    member _.Modules = lookup.Keys :> IReadOnlyCollection<string>
    member _.Imports = imports

type OperandType =
    | ValType of ValType
    | UnknownType

module OperandType =
    let isNumType ty =
        match ty with
        | ValType(ValType.Num _) | UnknownType -> true
        | ValType(ValType.Ref _ | ValType.Vec _) -> false

    let isVecType ty =
        match ty with
        | ValType(ValType.Vec _) | UnknownType -> true
        | ValType(ValType.Ref _ | ValType.Num _) -> false

    let isRefType ty =
        match ty with
        | ValType(ValType.Ref _) | UnknownType -> true
        | ValType(ValType.Num _ | ValType.Vec _) -> false

    let i32 = ValType ValType.i32
    let i64 = ValType ValType.i64
    let f32 = ValType ValType.f32
    let f64 = ValType ValType.f64
    let funcref = ValType ValType.funcref
    let externref = ValType ValType.externref

    let fromRefType ty =
        match ty with
        | FuncRef -> funcref
        | ExternRef -> externref

type ValidInstruction = { Instruction: Instruction; Unreachable: bool }

[<Sealed>]
type ValidExpression =
    val private source: ImmutableArray<Instruction>
    val private parameterTypes: ImmutableArray<ValType>
    val private localTypes: ImmutableArray<ValType>
    val private resultTypes: ImmutableArray<ValType>
    val mutable private instructions: ImmutableArray<ValidInstruction>
    val mutable private maximumIntroducedBlockCount: int32

    internal new(source, parameterTypes, localTypes, resultTypes) =
        { source = source
          instructions = Unchecked.defaultof<_>
          parameterTypes = parameterTypes
          localTypes = localTypes
          resultTypes = resultTypes
          maximumIntroducedBlockCount = 0 }

    member expr.SetInstructions instructions = expr.instructions <- instructions
    member expr.SetMaximumIntroducedBlockCount maximum = expr.maximumIntroducedBlockCount <- maximum

    static member inline private EnsureNotDefault(items: ImmutableArray<'a>) =
        if items.IsDefault then invalidOp "expression was not set"
        items

    member expr.Source = expr.source
    member expr.Instructions = ValidExpression.EnsureNotDefault expr.instructions
    member expr.ParameterTypes = expr.parameterTypes
    member expr.LocalTypes = expr.localTypes
    member expr.ResultTypes = expr.resultTypes
    member expr.MaximumIntroducedBlockCount = expr.maximumIntroducedBlockCount

    member expr.TryGetLocal(LocalIdx index, variableType: outref<_>) =
        if index < expr.parameterTypes.Length then
            variableType <- expr.parameterTypes[index]
            true
        else if index - expr.parameterTypes.Length < expr.localTypes.Length then
            variableType <- expr.localTypes[index - expr.parameterTypes.Length]
            true
        else
            false

type Function = { Type: FuncType; Body: ValidExpression }

[<RequireQualifiedAccess>]
type Global = { Type: GlobalType; Value: ValidExpression }

[<RequireQualifiedAccess>]
type AnyFunction =
    | Defined of int * Function
    | Import of int * FunctionImport

    member this.Type =
        match this with
        | Defined(_, f) -> f.Type
        | Import(_, f) -> f.Type

[<RequireQualifiedAccess>]
type AnyMemory =
    | Defined of int * Limits
    | Import of int * MemoryImport

    member this.Limits =
        match this with
        | Defined(_, limits) -> limits
        | Import(_, import) -> import.Limits

[<RequireQualifiedAccess>]
type AnyTable =
    | Defined of int * TableType
    | Import of int * TableImport

    member this.Type =
        match this with
        | Defined(_, ty) -> ty
        | Import(_, import) -> import.Type

[<RequireQualifiedAccess>]
type AnyGlobal =
    | Defined of int * Global
    | Import of int * GlobalImport

    member this.Type =
        match this with
        | Defined(_, defined) -> defined.Type
        | Import(_, import) -> import.Type

[<RequireQualifiedAccess>]
type ModuleExport =
    | Function of FuncIdx * AnyFunction
    | Table of TableIdx * AnyTable
    | Memory of MemIdx * AnyMemory
    | Global of GlobalIdx * AnyGlobal

[<Sealed>]
type ModuleExportLookup internal
    (
        memories: Dictionary<MemIdx, string>,
        functions: Dictionary<FuncIdx, string>,
        tables: Dictionary<TableIdx, string>,
        lookup: Dictionary<string, ModuleExport>
    )
    =
    member _.GetMemoryName(index, name: outref<_>) = memories.TryGetValue(index, &name)
    member _.GetFunctionName(index, name: outref<_>) = functions.TryGetValue(index, &name)
    member _.GetTableName(index, name: outref<_>) = tables.TryGetValue(index, &name)
    member _.Item with get name = lookup[name]

[<RequireQualifiedAccess>]
type ValidActiveData = { Memory: MemIdx; Offset: ValidExpression }

[<RequireQualifiedAccess>]
type ValidData = { Bytes: ImmutableArray<byte>; Mode: ValidActiveData voption }
