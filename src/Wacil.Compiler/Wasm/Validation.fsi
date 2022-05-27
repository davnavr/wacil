namespace Wacil.Compiler.Wasm.Validation

open System.Collections.Immutable

open Wacil.Compiler.Wasm

[<NoComparison; StructuralEquality>]
type ValidModule =
    internal
        { memories: ImmutableArray<Format.Limits> }

type ValidModule with
    member Memories: ImmutableArray<Format.Limits>

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type Error =
    | MultiMemoryNotSupported

    override ToString: unit -> string

[<RequireQualifiedAccess>]
module Validate =
    val fromModuleSections: sections: ImmutableArray<Format.Section> -> Result<ValidModule, Error>
