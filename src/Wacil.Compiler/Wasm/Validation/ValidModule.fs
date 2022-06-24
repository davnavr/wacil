namespace Wacil.Compiler.Wasm.Validation

open System.Collections.Immutable

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

        failwith "A"

        ValidModule(
            custom = contents.CustomSections.ToImmutableArray(),
            types = types,
            imports = imports,
            functions = functions,
            tables = tables,
            memories = memories,
            globals = globals,
            exports = exports,
            start = ValueOption.map Checked.int32 builder.Start,
            data = data
        )
