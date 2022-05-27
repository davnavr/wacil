namespace Wacil.Compiler.Wasm.Validation

open System.Collections.Immutable

open Wacil.Compiler.Helpers
open Wacil.Compiler.Helpers.Collections

open Wacil.Compiler.Wasm.Format

type ValidModuleBuilder =
    { CustomSections: ArrayBuilder<Custom>
      Memories: ImmutableArray<Limits> voption }

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

    override this.ToString() =
        match this with
        | MultiMemoryNotSupported ->
            "Multiple memories in a WebAssembly module are not yet supported. See the proposal text at https://github.com/WebAssembly/multi-memory for more information"

[<RequireQualifiedAccess>]
module Validate =
    let fromModuleSections (sections: ImmutableArray<Section>) =
        let builder =
            { CustomSections = ArrayBuilder()
              Memories = ValueNone }

        let error = ValueNone
        let mutable moduleSectionEnumerator = sections.GetEnumerator()

        while error.IsNone && moduleSectionEnumerator.MoveNext() do
            ()
            
        match error with
        | ValueSome error' -> Error(error')
        | ValueNone -> Ok(ValidModule(
            custom = builder.CustomSections.ToImmutableArray(),
            memories = builder.Memories
        ))
