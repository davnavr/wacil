namespace Wacil.Compiler.Wasm.Validation

open System.Collections.Immutable

open Wacil.Compiler.Helpers
open Wacil.Compiler.Helpers.Collections

open Wacil.Compiler.Wasm.Format

type ValidModuleBuilder =
    { mutable CustomSections: ArrayBuilder<Custom>
      mutable Memories: ImmutableArray<Limits> voption }

[<Sealed>]
type ValidModule
    (
        custom: ImmutableArray<Custom>,
        memories: ImmutableArray<Limits> voption
    )
    =
    member _.CustomSections = custom
    member val Memories = ValueOption.defaultValue ImmutableArray.Empty memories

type Error =
    | MultiMemoryNotSupported
    | DuplicateSection of id: SectionId

    override this.ToString() =
        match this with
        | MultiMemoryNotSupported ->
            "Multiple memories in a WebAssembly module are not yet supported. See the proposal text at https://github.com/WebAssembly/multi-memory for more information"
        | DuplicateSection id ->
            sprintf "A %O section already exists" id

[<RequireQualifiedAccess>]
module Validate =
    let fromModuleSections (sections: ImmutableArray<Section>) =
        let builder =
            { CustomSections = ArrayBuilder()
              Memories = ValueNone }

        let mutable error = ValueNone
        let mutable moduleSectionEnumerator = sections.GetEnumerator()

        while error.IsNone && moduleSectionEnumerator.MoveNext() do
            match moduleSectionEnumerator.Current with
            | Section.Custom custom -> builder.CustomSections.Add custom
            | Section.Memory memory when builder.Memories.IsNone -> builder.Memories <- ValueSome memory
            | Section.Memory _ -> error <- ValueSome(Error.DuplicateSection SectionId.Memory)
            
        match error with
        | ValueSome error' -> Error(error')
        | ValueNone -> Ok(ValidModule(
            custom = builder.CustomSections.ToImmutableArray(),
            memories = builder.Memories
        ))
