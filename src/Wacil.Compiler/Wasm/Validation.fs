namespace Wacil.Compiler.Wasm.Validation

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open Wacil.Compiler.Helpers.Collections

open Wacil.Compiler.Wasm.Format

[<IsReadOnly; Struct; RequireQualifiedAccess; StructuralComparison; StructuralEquality>]
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
        | Type -> SectionId.Type
        | Import -> SectionId.Import
        | Function -> SectionId.Function
        | Table -> SectionId.Table
        | Memory -> SectionId.Memory
        | Global -> SectionId.Global
        | Export -> SectionId.Export
        | Start -> SectionId.Start
        | Element -> SectionId.Element
        | DataCount -> SectionId.DataCount
        | Code -> SectionId.Code
        | Data -> SectionId.Data

[<IsByRefLike; Struct; NoComparison; StructuralEquality>]
type ValidModuleBuilder =
    { mutable CustomSections: ArrayBuilder<Custom>
      mutable Types: ImmutableArray<FuncType> voption
      mutable Imports: ImmutableArray<Import> voption
      mutable Functions: ImmutableArray<Index> voption
      mutable Tables: ImmutableArray<TableType> voption
      mutable Memories: ImmutableArray<Limits> voption
      mutable Globals: ImmutableArray<Global> voption
      mutable Exports: ImmutableArray<Export> voption
      mutable Start: Index voption
      mutable Element: ImmutableArray<Element> voption 
      mutable Code: ImmutableArray<Code> voption
      mutable DataCount: uint32 voption
      mutable Data: ImmutableArray<Data> voption }

type FunctionImport = { Name: string; Type: FuncType }

type TableImport = { Name: string; Type: TableType }

type MemoryImport = { Name: string; Limits: Limits }

type GlobalImport = { Name: string; Type: GlobalType }

type ModuleImports =
    { Functions: ImmutableArray<FunctionImport>
      Tables: ImmutableArray<TableImport>
      Memories: ImmutableArray<MemoryImport>
      Globals: ImmutableArray<GlobalImport> }

[<Sealed>]
type ModuleImportLookup (lookup: Dictionary<string, ModuleImports>) =
    member _.Item with get (moduleImportName: string) =
        if isNull lookup then
            raise(KeyNotFoundException "modules does not contain any imports")
        lookup[moduleImportName]

    interface IReadOnlyDictionary<string, ModuleImports> with
        member this.Item with get key = this[key]
        member _.Keys = lookup.Keys
        member _.Values = lookup.Values
        member _.Count = lookup.Count
        member _.ContainsKey key = lookup.ContainsKey key
        member _.TryGetValue(key: string, value: byref<ModuleImports>) = lookup.TryGetValue(key, &value)
        member _.GetEnumerator() = if isNull lookup then Seq.empty.GetEnumerator() else lookup.GetEnumerator() :> IEnumerator<_>
        member _.GetEnumerator() = lookup.GetEnumerator() :> System.Collections.IEnumerator

[<Sealed>]
type ValidModule
    (
        custom: ImmutableArray<Custom>,
        types: ImmutableArray<FuncType>,
        imports: ModuleImportLookup,
        memories: ImmutableArray<Limits>
    )
    =
    member _.CustomSections = custom
    member _.Types = types
    member _.Imports = imports

    member _.Memories = memories

type Error =
    | MultiMemoryNotSupported
    | DuplicateSection of id: SectionId
    | InvalidSectionOrder of section: SectionId * next: SectionId

    override this.ToString() =
        match this with
        | MultiMemoryNotSupported ->
            "Multiple memories in a WebAssembly module are not yet supported. See the proposal text at https://github.com/WebAssembly/multi-memory for more information"
        | DuplicateSection id ->
            sprintf "A %O section already exists" id
        | InvalidSectionOrder(section, next) ->
            sprintf "The %O section must be placed before the %O section" section next

[<RequireQualifiedAccess>]
module Validate =
    let fromModuleSections (sections: ImmutableArray<Section>) =
        let mutable builder =
            { CustomSections = ArrayBuilder()
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

        let mutable order = SectionOrder.Type
        let mutable error = ValueNone
        let mutable moduleSectionEnumerator = sections.GetEnumerator()

        while error.IsNone && moduleSectionEnumerator.MoveNext() do
            match moduleSectionEnumerator.Current with
            | Section.Custom custom -> builder.CustomSections.Add custom
            | Section.Type types ->
                if order > SectionOrder.Type then error <- ValueSome(InvalidSectionOrder(SectionId.Type, order.Id))
                else if builder.Types.IsSome then error <- ValueSome(DuplicateSection SectionId.Type)
                else
                    order <- SectionOrder.Import
                    builder.Types <- ValueSome types
            | Section.Import imports ->
                if order > SectionOrder.Import then error <- ValueSome(InvalidSectionOrder(SectionId.Import, order.Id))
                else if builder.Imports.IsSome then error <- ValueSome(DuplicateSection SectionId.Import)
                else
                    order <- SectionOrder.Function
                    builder.Imports <- ValueSome imports
            | Section.Function functions ->
                if order > SectionOrder.Function then error <- ValueSome(InvalidSectionOrder(SectionId.Function, order.Id))
                else if builder.Functions.IsSome then error <- ValueSome(DuplicateSection SectionId.Function)
                else
                    order <- SectionOrder.Table
                    builder.Functions <- ValueSome functions
            | Section.Table tables ->
                if order > SectionOrder.Table then error <- ValueSome(InvalidSectionOrder(SectionId.Table, order.Id))
                else if builder.Tables.IsSome then error <- ValueSome(DuplicateSection SectionId.Table)
                else
                    order <- SectionOrder.Memory
                    builder.Tables <- ValueSome tables
            | Section.Memory memory ->
                if order > SectionOrder.Memory then error <- ValueSome(InvalidSectionOrder(SectionId.Memory, order.Id))
                else if builder.Memories.IsSome then error <- ValueSome(DuplicateSection SectionId.Memory)
                else if memory.Length > 1 then error <- ValueSome MultiMemoryNotSupported
                else
                    order <- SectionOrder.Global
                    builder.Memories <- ValueSome memory
            | Section.Global globals ->
                if order > SectionOrder.Global then error <- ValueSome(InvalidSectionOrder(SectionId.Global, order.Id))
                else if builder.Globals.IsSome then error <- ValueSome(DuplicateSection SectionId.Global)
                else
                    order <- SectionOrder.Export
                    builder.Globals <- ValueSome globals
            | Section.Export exports ->
                if order > SectionOrder.Export then error <- ValueSome(InvalidSectionOrder(SectionId.Export, order.Id))
                else if builder.Exports.IsSome then error <- ValueSome(DuplicateSection SectionId.Export)
                else
                    order <- SectionOrder.Start
                    builder.Exports <- ValueSome exports
            | Section.Start start ->
                if order > SectionOrder.Start then error <- ValueSome(InvalidSectionOrder(SectionId.Start, order.Id))
                else if builder.Start.IsSome then error <- ValueSome(DuplicateSection SectionId.Start)
                else
                    order <- SectionOrder.Element
                    builder.Start <- ValueSome start
            | Section.Element elements ->
                if order > SectionOrder.Element then error <- ValueSome(InvalidSectionOrder(SectionId.Element, order.Id))
                else if builder.Element.IsSome then error <- ValueSome(DuplicateSection SectionId.Element)
                else
                    order <- SectionOrder.DataCount
                    builder.Element <- ValueSome elements
            | Section.DataCount count ->
                if order > SectionOrder.DataCount then error <- ValueSome(InvalidSectionOrder(SectionId.DataCount, order.Id))
                else if builder.DataCount.IsSome then error <- ValueSome(DuplicateSection SectionId.DataCount)
                else
                    order <- SectionOrder.Code
                    builder.DataCount <- ValueSome count
            | Section.Code code ->
                if order > SectionOrder.Code then error <- ValueSome(InvalidSectionOrder(SectionId.Code, order.Id))
                else if builder.Code.IsSome then error <- ValueSome(DuplicateSection SectionId.Code)
                else
                    order <- SectionOrder.Data
                    builder.Code <- ValueSome code
            | Section.Data data ->
                if builder.Data.IsSome then error <- ValueSome(DuplicateSection SectionId.Data)
                else
                    order <- SectionOrder.Export
                    builder.Data <- ValueSome data

        let types = ValueOption.defaultValue ImmutableArray.Empty builder.Types

        // let moduleImports = ValueOption.defaultValue ImmutableArray.Empty builder.Imports
        // let moduleImportLookup = Dictionary(capacity = moduleImports.Length)
        // for import in moduleImports do
        //     let entries =
        //         match moduleImportLookup.TryGetValue import.Module with
        //         | true, entries' -> entries'
        //         | false, _ ->
        //             let entries' = Dictionary()
        //             moduleImportLookup[import.Module] <- entries'
        //             entries'

        //     let duplicates =
        //         match entries.TryGetValue import.Name with
        //         | true, duplicates' -> duplicates'
        //         | false, _ ->
        //             let duplicates' = List()
        //             entries[import.Name] <- duplicates'
        //             duplicates'

        //     ()
        let imports =
            // Group all imports by module name
            let moduleImportLookup =
                let imports = ValueOption.defaultValue ImmutableArray.Empty builder.Imports
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
                let mutable functions = ArrayBuilder()
                let mutable tables = ArrayBuilder()
                let mutable memories = ArrayBuilder()
                let mutable globals = ArrayBuilder()

                for KeyValue(importModuleName, moduleImports) in moduleImportLookup do
                    functions.Clear()
                    tables.Clear()
                    memories.Clear()
                    globals.Clear()

                    for import in moduleImports do
                        match import.Description with
                        | ImportDesc.Func index ->
                            functions.Add { FunctionImport.Name = import.Name; Type = types[Checked.int32 index] }
                        | ImportDesc.Table ty -> tables.Add { TableImport.Name = import.Name; Type = ty }
                        | ImportDesc.Mem limits -> memories.Add { MemoryImport.Name = import.Name; Limits = limits }
                        | ImportDesc.Global ty -> globals.Add { GlobalImport.Name = import.Name; Type = ty }

                    actualModuleImports[importModuleName] <- 
                        { ModuleImports.Functions = functions.ToImmutableArray()
                          Tables = tables.ToImmutableArray()
                          Memories = memories.ToImmutableArray()
                          Globals = globals.ToImmutableArray() }

                ModuleImportLookup(actualModuleImports)
            else
                ModuleImportLookup(null)

        // TODO: Loop through each import and create a dictionary of dictionaries
        // TODO: Loop through each export similarly as the imports to create a dictionary
        //let export = Dictionary<string, _>(capacity = )

        // TODO: In sections where only constant expressions are allowed, loop through them to check they are valid

        // TODO: Loop through functions to check that they are valid
        
        match error with
        | ValueSome error' -> Error(error')
        | ValueNone -> Ok(ValidModule(
            custom = builder.CustomSections.ToImmutableArray(),
            types = types,
            imports = imports,

            memories = ValueOption.defaultValue ImmutableArray.Empty builder.Memories
        ))
