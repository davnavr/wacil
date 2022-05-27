namespace Wacil.Compiler.Wasm.Validation

open System.Collections.Immutable

open Wacil.Compiler.Wasm.Format

type ValidModule =
    { memories: ImmutableArray<Limits> }
    
    member this.Memories = this.memories

type Error =
    | MultiMemoryNotSupported

    override this.ToString() =
        match this with
        | MultiMemoryNotSupported ->
            "Multiple memories in a WebAssembly module are not yet supported. See the proposal text at https://github.com/WebAssembly/multi-memory for more information"

[<RequireQualifiedAccess>]
module Validate =
    let fromModuleSections (sections: ImmutableArray<Section>) =
        let validated =
            { memories = ImmutableArray.Empty }

        failwith "A"
