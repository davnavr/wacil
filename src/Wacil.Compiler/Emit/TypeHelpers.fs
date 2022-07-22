[<RequireQualifiedAccess>]
module Wacil.Compiler.Emit.TypeHelpers

open AsmResolver.DotNet.Signatures.Types

let isByRef (ty: TypeSignature) =
    match ty with
    | :? ByReferenceTypeSignature -> true
    | _ -> false
