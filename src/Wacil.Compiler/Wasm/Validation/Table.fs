namespace Wacil.Compiler.Wasm.Validation.Table

open System.Collections.Immutable
open System.Collections.Generic

open Wacil.Compiler.Wasm
open Wacil.Compiler.Wasm.Format

type Index = int

[<RequireQualifiedAccess>]
type FunctionImport = { Name: string; Type: FuncType }

[<RequireQualifiedAccess>]
type TableImport = { Name: string; Type: TableType }

[<RequireQualifiedAccess>]
type MemoryImport = { Name: string; Limits: Limits }

[<RequireQualifiedAccess>]
type GlobalImport = { Name: string; Type: GlobalType }

[<RequireQualifiedAccess>]
type ModuleImports =
    { Functions: ImmutableArray<FunctionImport>
      Tables: ImmutableArray<TableImport>
      Memories: ImmutableArray<MemoryImport>
      Globals: ImmutableArray<GlobalImport> }

[<Sealed>]
type ModuleImportLookup internal
    (
        lookup: Dictionary<string, ModuleImports>,
        functions: ImmutableArray<struct(string * FunctionImport)>
    )
    =
    member _.Item with get (moduleImportName: string) =
        if isNull lookup then
            raise(KeyNotFoundException "modules does not contain any imports")
        lookup[moduleImportName]

    member _.Count = if isNull lookup then 0 else lookup.Count

    member _.Functions = functions

    interface IReadOnlyDictionary<string, ModuleImports> with
        member this.Item with get key = this[key]
        member _.Keys = lookup.Keys
        member _.Values = lookup.Values
        member this.Count = this.Count
        member _.ContainsKey key = lookup.ContainsKey key
        member _.TryGetValue(key: string, value: byref<ModuleImports>) = lookup.TryGetValue(key, &value)
        member _.GetEnumerator() = if isNull lookup then Seq.empty.GetEnumerator() else lookup.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = lookup.GetEnumerator() :> System.Collections.IEnumerator

type OperandType =
    | ValType of ValType
    | UnknownType

module OperandType =
    let (|IsNumType|) ty =
        match ty with
        | ValType(ValType.Num _) | UnknownType -> true
        | ValType(ValType.Ref _ | ValType.Vec _) -> false

    let (|IsVecType|) ty =
        match ty with
        | ValType(ValType.Vec _) | UnknownType -> true
        | ValType(ValType.Ref _ | ValType.Num _) -> false

    let (|IsRefType|) ty =
        match ty with
        | ValType(ValType.Ref _) | UnknownType -> true
        | ValType(ValType.Num _ | ValType.Vec _) -> false

    let i32 = ValType(ValType.Num I32)
    let i64 = ValType(ValType.Num I64)
    let f32 = ValType(ValType.Num F32)
    let f64 = ValType(ValType.Num F64)
    let singleI32 = ImmutableArray.Create i32
    let singleI64 = ImmutableArray.Create i64
    let singleF32 = ImmutableArray.Create f32
    let singleF64 = ImmutableArray.Create f64

type ValidInstruction =
    { Instruction: Instruction
      PoppedTypes: ImmutableArray<OperandType>
      PushedTypes: ImmutableArray<OperandType>
      Unreachable: bool }

[<Sealed>]
type ValidExpression =
    val private source: ImmutableArray<Instruction>
    val private parameterTypes: ImmutableArray<ValType>
    val private localTypes: ImmutableArray<ValType>
    val private resultTypes: ImmutableArray<ValType>
    val mutable private instructions: ImmutableArray<ValidInstruction>

    internal new(source, parameterTypes, localTypes, resultTypes) =
        { source = source
          instructions = Unchecked.defaultof<_>
          parameterTypes = parameterTypes
          localTypes = localTypes
          resultTypes = resultTypes }

    member expr.SetInstructions instructions = expr.instructions <- instructions

    static member inline private EnsureNotDefault(items: ImmutableArray<'a>) =
        if items.IsDefault then invalidOp "expression was not set"
        items

    member expr.Source = expr.source
    member expr.Instructions = ValidExpression.EnsureNotDefault expr.instructions
    member expr.ParameterTypes = expr.parameterTypes
    member expr.LocalTypes = expr.localTypes
    member expr.ResultTypes = expr.resultTypes

    member expr.TryGetLocal(index, variableType: outref<_>) =
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
type ModuleExport =
    | Function of Function
    | Table
    | Memory of Index
    | Global

[<Sealed>]
type ModuleExportLookup internal
    (
        memories: Dictionary<Index, string>,
        functions: Dictionary<Index, string>,
        tables: Dictionary<Index, string>,
        lookup: Dictionary<string, ModuleExport>
    )
    =
    member _.GetMemoryName(index, name: outref<_>) = memories.TryGetValue(index, &name)
    member _.GetFunctionName(index, name: outref<_>) = functions.TryGetValue(index, &name)
    member _.GetTableName(index, name: outref<_>) = tables.TryGetValue(index, &name)
    member _.Item with get name = lookup[name]

[<RequireQualifiedAccess>]
type ValidActiveData = { Memory: Index; Offset: ValidExpression }

[<RequireQualifiedAccess>]
type ValidData = { Bytes: ImmutableArray<byte>; Mode: ValidActiveData voption }
