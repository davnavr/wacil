namespace Wacil.Compiler.Wasm.Validation.Table

open System.Collections.Immutable
open System.Collections.Generic

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
type ModuleImportLookup
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

// TODO: Label and instruction stuff

[<Sealed>]
type ValidExpression internal () = class end

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
type ModuleExportLookup
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
