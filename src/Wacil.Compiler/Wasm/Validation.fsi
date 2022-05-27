namespace Wacil.Compiler.Wasm.Validation

open System.Collections.Immutable

open Wacil.Compiler.Wasm

[<Sealed>]
type ValidModule =
    member CustomSections: ImmutableArray<Format.Custom>
    member Memories: ImmutableArray<Format.Limits>

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type Error =
    | MultiMemoryNotSupported

    override ToString: unit -> string

[<RequireQualifiedAccess>]
module Validate =
    val fromModuleSections: sections: ImmutableArray<Format.Section> -> Result<ValidModule, Error>
