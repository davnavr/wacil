namespace Wacil.Compiler.Wasm.Validation

open System.Collections.Immutable
open System.Collections.Generic

open Wacil.Compiler.Helpers
open Wacil.Compiler.Helpers.Collections

open Wacil.Compiler.Wasm
open Wacil.Compiler.Wasm.Validation.Table

[<Sealed>]
type ValidModule
    (
        custom: ImmutableArray<Custom>,
        types: ImmutableArray<FuncType>,
        functions: ImmutableArray<Function>,
        tables: ImmutableArray<TableType>,
        imports: ModuleImportLookup,
        memories: ImmutableArray<Limits>,
        globals: ImmutableArray<Global>,
        exports: ModuleExportLookup,
        start: Table.Index voption,
        data: ImmutableArray<ValidData>
    )
    =
    member _.CustomSections = custom
    member _.Types = types
    member _.Imports = imports
    member _.Functions = functions
    member _.Tables = tables
    member _.Memories = memories
    member _.Globals = globals
    member _.Exports = exports
    member _.Start = start
    member _.Data = data

type ValidationException =
    inherit System.Exception

    new (message: string) = { inherit System.Exception(message) }

type DuplicateSectionException (section: Format.SectionId) =
    inherit ValidationException(sprintf "The %O section already exists" section)

    member _.Section = section

type InvalidSectionOrderException (current: Format.SectionId, next: Format.SectionId) =
    inherit ValidationException(sprintf "The %O section must be placed before the %O section" current next)

    member _.CurrentSection = current
    member _.NextSection = next

type FunctionSectionCountException (section: Format.SectionId, expected: int, actual: int) =
    inherit
        ValidationException(
            sprintf
                "The %O section used for functions was expected to contain %i items, but got %i items"
                section
                expected
                actual
        )

    member _.Section = section

type DataSegmentCountException (expected: int, actual: int) =
    inherit ValidationException(sprintf "Expected %i data segments but got %i" expected actual)

    member _.Expected = expected
    member _.Actual = actual

type DuplicateExportException (name: string) =
    inherit ValidationException(sprintf "An export corresponding to the name \"%s\" already exists" name)

    member _.Name = name

module Validate =
    let errwith format = Printf.kprintf (fun msg -> raise(ValidationException msg)) format

    let multiMemoryNotSupported() =
        let e = System.NotSupportedException "Multiple memories are not yet supported by the Wacil compiler"
        e.HelpLink <- "https://github.com/WebAssembly/multi-memory"
        raise e

    [<Struct; RequireQualifiedAccess; StructuralComparison; StructuralEquality>]
    type SectionOrder =
        | Type
        | Import
        | Function
        | Table
        | Memory
        | Global
        | Export
        | Start
        | Element
        | DataCount
        | Code
        | Data

        member this.Id =
            match this with
            | Type -> Format.SectionId.Type
            | Import -> Format.SectionId.Import
            | Function -> Format.SectionId.Function
            | Table -> Format.SectionId.Table
            | Memory -> Format.SectionId.Memory
            | Global -> Format.SectionId.Global
            | Export -> Format.SectionId.Export
            | Start -> Format.SectionId.Start
            | Element -> Format.SectionId.Element
            | DataCount -> Format.SectionId.DataCount
            | Code -> Format.SectionId.Code
            | Data -> Format.SectionId.Data

    [<System.Runtime.CompilerServices.IsByRefLike; Struct; NoComparison; NoEquality>]
    type ValidModuleBuilder =
        { mutable CustomSections: ArrayBuilder<Format.Custom>
          mutable Types: ImmutableArray<Format.FuncType> voption
          mutable Imports: ImmutableArray<Format.Import> voption
          mutable Functions: ImmutableArray<Format.Index> voption
          mutable Tables: ImmutableArray<Format.TableType> voption
          mutable Memories: ImmutableArray<Format.Limits> voption
          mutable Globals: ImmutableArray<Format.Global> voption
          mutable Exports: ImmutableArray<Format.Export> voption
          mutable Start: Format.Index voption
          mutable Element: ImmutableArray<Format.Element> voption 
          mutable Code: ImmutableArray<Format.Code> voption
          mutable DataCount: uint32 voption
          mutable Data: ImmutableArray<Format.Data> voption }

    let fromModuleSections (sections: ImmutableArray<Format.Section>) =
        let mutable contents =
            { CustomSections = ArrayBuilder<_>.Create()
              Types = ValueNone
              Imports = ValueNone
              Functions = ValueNone
              Tables = ValueNone
              Memories = ValueNone
              Globals = ValueNone
              Exports = ValueNone
              Start = ValueNone
              Element = ValueNone
              DataCount = ValueNone
              Code = ValueNone
              Data = ValueNone }

        do // Check that sections are in the correct order
            let mutable order = SectionOrder.Type
            for s in sections do
                match s with
                | Format.Section.Custom custom -> contents.CustomSections.Add custom
                | Format.Section.Type types ->
                    if order > SectionOrder.Type then raise(InvalidSectionOrderException(Format.SectionId.Type, order.Id))
                    else if contents.Types.IsSome then raise(DuplicateSectionException Format.SectionId.Type)
                    else
                        order <- SectionOrder.Import
                        contents.Types <- ValueSome types
                | Format.Section.Import imports ->
                    if order > SectionOrder.Import then raise(InvalidSectionOrderException(Format.SectionId.Import, order.Id))
                    else if contents.Imports.IsSome then raise(DuplicateSectionException Format.SectionId.Import)
                    else
                        order <- SectionOrder.Function
                        contents.Imports <- ValueSome imports
                | Format.Section.Function functions ->
                    if order > SectionOrder.Function then
                        raise(InvalidSectionOrderException(Format.SectionId.Function, order.Id))
                    else if contents.Functions.IsSome then
                        raise(DuplicateSectionException Format.SectionId.Function)
                    else
                        order <- SectionOrder.Table
                        contents.Functions <- ValueSome functions
                | Format.Section.Table tables ->
                    if order > SectionOrder.Table then raise(InvalidSectionOrderException(Format.SectionId.Table, order.Id))
                    else if contents.Tables.IsSome then raise(DuplicateSectionException Format.SectionId.Table)
                    else
                        order <- SectionOrder.Memory
                        contents.Tables <- ValueSome tables
                | Format.Section.Memory memory ->
                    if order > SectionOrder.Memory then raise(InvalidSectionOrderException(Format.SectionId.Memory, order.Id))
                    else if contents.Memories.IsSome then raise(DuplicateSectionException Format.SectionId.Memory)
                    else if memory.Length > 1 then multiMemoryNotSupported()
                    else
                        order <- SectionOrder.Global
                        contents.Memories <- ValueSome memory
                | Format.Section.Global globals ->
                    if order > SectionOrder.Global then raise(InvalidSectionOrderException(Format.SectionId.Global, order.Id))
                    else if contents.Globals.IsSome then raise(DuplicateSectionException Format.SectionId.Global)
                    else
                        order <- SectionOrder.Export
                        contents.Globals <- ValueSome globals
                | Format.Section.Export exports ->
                    if order > SectionOrder.Export then raise(InvalidSectionOrderException(Format.SectionId.Export, order.Id))
                    else if contents.Exports.IsSome then raise(DuplicateSectionException Format.SectionId.Export)
                    else
                        order <- SectionOrder.Start
                        contents.Exports <- ValueSome exports
                | Format.Section.Start start ->
                    if order > SectionOrder.Start then raise(InvalidSectionOrderException(Format.SectionId.Start, order.Id))
                    else if contents.Start.IsSome then raise(DuplicateSectionException Format.SectionId.Start)
                    else
                        order <- SectionOrder.Element
                        contents.Start <- ValueSome start
                | Format.Section.Element elements ->
                    if order > SectionOrder.Element then raise(InvalidSectionOrderException(Format.SectionId.Element, order.Id))
                    else if contents.Element.IsSome then raise(DuplicateSectionException Format.SectionId.Element)
                    else
                        order <- SectionOrder.DataCount
                        contents.Element <- ValueSome elements
                | Format.Section.DataCount count ->
                    if order > SectionOrder.DataCount then
                        raise(InvalidSectionOrderException(Format.SectionId.DataCount, order.Id))
                    else if contents.DataCount.IsSome then
                        raise(DuplicateSectionException Format.SectionId.DataCount)
                    else
                        order <- SectionOrder.Code
                        contents.DataCount <- ValueSome count
                | Format.Section.Code code ->
                    if order > SectionOrder.Code then raise(InvalidSectionOrderException(Format.SectionId.Code, order.Id))
                    else if contents.Code.IsSome then raise(DuplicateSectionException Format.SectionId.Code)
                    else
                        order <- SectionOrder.Data
                        contents.Code <- ValueSome code
                | Format.Section.Data data ->
                    if contents.Data.IsSome then raise(DuplicateSectionException Format.SectionId.Data)
                    else
                        order <- SectionOrder.Export
                        contents.Data <- ValueSome data

        let inline itemsOrEmpty items = ValueOption.defaultValue ImmutableArray.Empty items

        // The order used to validate a module's sections as in the specification is not exactly followed here

        let types = itemsOrEmpty contents.Types
        let tables = itemsOrEmpty contents.Tables
        let memories = itemsOrEmpty contents.Memories

        let imports =
            // Group all imports by module name
            let moduleImportLookup =
                let imports = itemsOrEmpty contents.Imports
                let lookup = if imports.IsEmpty then null else Dictionary(capacity = imports.Length)
                for import in imports do
                    // Loop is skipped if lookup is null
                    let entries =
                        match lookup.TryGetValue import.Module with
                        | true, entries' -> entries'
                        | false, _ ->
                            let entries' = List()
                            lookup[import.Module] <- entries'
                            entries'

                    entries.Add(import)
                lookup

            if not(isNull moduleImportLookup) then
                let actualModuleImports = Dictionary(capacity = moduleImportLookup.Count)
                let mutable functions = ArrayBuilder<FunctionImport>.Create()
                let mutable tables = ArrayBuilder<TableImport>.Create()
                let mutable memories = ArrayBuilder<MemoryImport>.Create()
                let mutable globals = ArrayBuilder<GlobalImport>.Create()
                let mutable allFunctions = ArrayBuilder<_>.Create()

                for KeyValue(importModuleName, moduleImports) in moduleImportLookup do
                    functions.Clear()
                    tables.Clear()
                    memories.Clear()
                    globals.Clear()

                    for import in moduleImports do
                        match import.Description with
                        | Format.ImportDesc.Func index ->
                            let func = { FunctionImport.Name = import.Name; FunctionImport.Type = types[Checked.int32 index] }
                            functions.Add func
                            allFunctions.Add(struct(importModuleName, func))
                        | Format.ImportDesc.Table ty -> tables.Add { TableImport.Name = import.Name; TableImport.Type = ty }
                        | Format.ImportDesc.Mem limits -> memories.Add { MemoryImport.Name = import.Name; Limits = limits }
                        | Format.ImportDesc.Global ty -> globals.Add { GlobalImport.Name = import.Name; Type = ty }

                    actualModuleImports[importModuleName] <- 
                        { ModuleImports.Functions = functions.ToImmutableArray()
                          ModuleImports.Tables = tables.ToImmutableArray()
                          ModuleImports.Memories = memories.ToImmutableArray()
                          ModuleImports.Globals = globals.ToImmutableArray() }

                ModuleImportLookup(
                    actualModuleImports,
                    allFunctions.ToImmutableArray()
                )
            else
                ModuleImportLookup(null, ImmutableArray.Empty)

        let globals =
            let moduleGlobalDefinitions = itemsOrEmpty contents.Globals
            let mutable globals = Array.zeroCreate moduleGlobalDefinitions.Length
            for i = 0 to globals.Length - 1 do
                let glbl = moduleGlobalDefinitions[i]
                globals[i] <-
                    { Global.Type = glbl.Type
                      Global.Value =
                        { ValidExpression.Source = glbl.Expression
                          Expression = Unchecked.defaultof<_>
                          BranchTargets = Unchecked.defaultof<_> } }
            Unsafe.Array.toImmutable globals

        let functions =
            let moduleFunctionTypes = itemsOrEmpty contents.Functions
            let moduleFunctionBodies = itemsOrEmpty contents.Code
            let expectedFunctionCount = moduleFunctionTypes.Length
            let mutable localTypesBuilder = ArrayBuilder<Format.ValType>.Create()

            if expectedFunctionCount <> moduleFunctionBodies.Length then
                raise(FunctionSectionCountException(Format.SectionId.Code, expectedFunctionCount, moduleFunctionBodies.Length))
                
            let mutable moduleFunctionDefinitions = ArrayBuilder<Function>.Create expectedFunctionCount

            for i = 0 to expectedFunctionCount - 1 do
                let body = moduleFunctionBodies[i]
                
                localTypesBuilder.Clear()
                for local in body.Locals do
                    for i = 0 to int local.Count - 1 do
                        localTypesBuilder.Add local.Type
                
                moduleFunctionDefinitions.Add
                    { Function.Type = types[moduleFunctionTypes[i] |> Checked.int32]
                      LocalTypes = localTypesBuilder.ToImmutableArray()
                      Body =
                        { ValidExpression.Source = body.Body
                          Expression = Unchecked.defaultof<_>
                          BranchTargets = Unchecked.defaultof<_> } }

            moduleFunctionDefinitions.ToImmutableArray()

        let data =
            let data = itemsOrEmpty contents.Data
            let expectedDataCount = ValueOption.defaultValue data.Length (ValueOption.map Checked.int32 contents.DataCount)
            // Some tools such as wat2wasm omit the data count even when data segments are present
            // A better way to validate the counts would be to use the expectedDataCount when validating the code before this point
            if expectedDataCount <> data.Length then
                raise(DataSegmentCountException(expectedDataCount, data.Length))

            let mutable segments = Array.zeroCreate data.Length
            for i = 0 to data.Length - 1 do
                let dataSegment = data[i]
                segments[i] <-
                    { ValidData.Bytes = dataSegment.Bytes
                      ValidData.Mode =
                        match dataSegment.Mode with
                        | Format.DataMode.Passive -> ValueNone
                        | Format.DataMode.Active(memory, offset) ->
                            ValueSome
                                { ValidActiveData.Memory = memory
                                  ValidActiveData.Offset =
                                    { ValidExpression.Source = offset
                                      Expression = Unchecked.defaultof<_>
                                      BranchTargets = Unchecked.defaultof<_> } } }
            Unsafe.Array.toImmutable segments

        let exports =
            let exports = ValueOption.defaultValue ImmutableArray.Empty contents.Exports
            let lookup = Dictionary(exports.Length, System.StringComparer.Ordinal)
            let memoryNames = Dictionary() // TODO: Use arrays in export lookup
            let functionNames = Dictionary()
            let tableNames = Dictionary()
            for e in exports do
                match lookup.TryGetValue e.Name with
                | true, _ -> raise(DuplicateExportException e.Name)
                | false, _ ->
                    lookup[e.Name] <-
                        match e.Description with
                        | Format.ExportDesc.Func index ->
                            functionNames.Add(Checked.int32 index, e.Name)
                            let i = Checked.int32 index - imports.Functions.Length
                            if i < 0 then
                                failwith "TODO: Fix, attempt was made to exprot a function import. Is this behavior allowed?"
                            ModuleExport.Function functions[i]
                        | Format.ExportDesc.Table index ->
                            tableNames.Add(Checked.int32 index, e.Name)
                            ModuleExport.Table
                        | Format.ExportDesc.Mem index ->
                            memoryNames.Add(Checked.int32 index, e.Name)
                            ModuleExport.Memory(Checked.int32 index)
                        | Format.ExportDesc.Global index ->
                            ModuleExport.Global
            ModuleExportLookup(memoryNames, functionNames, tableNames, lookup)

        // TODO: Validate expressions

        ValidModule(
            custom = contents.CustomSections.ToImmutableArray(),
            types = types,
            imports = imports,
            functions = functions,
            tables = tables,
            memories = memories,
            globals = globals,
            exports = exports,
            start = ValueOption.map Checked.int32 contents.Start,
            data = data
        )
