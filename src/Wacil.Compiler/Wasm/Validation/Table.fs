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

type ValidInstruction =
    { Instruction: Instruction
      PoppedTypes: ImmutableArray<OperandType>
      PushedTypes: ImmutableArray<OperandType>
      Unreachable: bool }

[<Sealed>]
type ValidExpression =
    val private source: ImmutableArray<Instruction>
    val mutable private instructions: ImmutableArray<ValidInstruction>
    val mutable private resultTypes: ImmutableArray<OperandType>

    internal new(source) =
        { source = source; instructions = Unchecked.defaultof<_>; resultTypes = Unchecked.defaultof<_> }

    member expr.SetInstructions instructions = expr.instructions <- instructions
    member expr.SetResultTypes resultTypes = expr.resultTypes <- resultTypes

    static member inline private EnsureNotDefault(items: ImmutableArray<'a>) =
        if items.IsDefault then invalidOp "expression was not set"
        items

    member expr.Source = expr.source
    member expr.Instructions = ValidExpression.EnsureNotDefault expr.instructions
    member expr.ResultTypes = ValidExpression.EnsureNotDefault expr.resultTypes

type Function = { Type: FuncType; LocalTypes: ImmutableArray<ValType>; Body: ValidExpression }

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
