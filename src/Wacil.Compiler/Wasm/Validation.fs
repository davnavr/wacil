namespace Wacil.Compiler.Wasm.Validation

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
      mutable Memories: ImmutableArray<Limits> voption }

[<Sealed>]
type ValidModule
    (
        custom: ImmutableArray<Custom>,
        types: ImmutableArray<FuncType>,
        memories: ImmutableArray<Limits>
    )
    =
    member _.CustomSections = custom
    member _.Types = types
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
              Memories = ValueNone }

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
            | Section.Memory memory ->
                if order > SectionOrder.Memory then error <- ValueSome(InvalidSectionOrder(SectionId.Memory, order.Id))
                else if builder.Memories.IsSome then error <- ValueSome(DuplicateSection SectionId.Memory)
                else
                    order <- SectionOrder.Global
                    builder.Memories <- ValueSome memory

        // TODO: Loop through each import and create a dictionary of dictionaries
        
        match error with
        | ValueSome error' -> Error(error')
        | ValueNone -> Ok(ValidModule(
            custom = builder.CustomSections.ToImmutableArray(),
            types = ValueOption.defaultValue ImmutableArray.Empty builder.Types,
            memories = ValueOption.defaultValue ImmutableArray.Empty builder.Memories
        ))
