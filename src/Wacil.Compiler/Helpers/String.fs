/// Contains helper functions for interacting with strings.
[<RequireQualifiedAccess>]
module Wacil.Compiler.Helpers.String

open System

let inline defaultValue defaultString (s: string) =
    if String.IsNullOrEmpty s then defaultString else s

let inline orEmpty (s: string) = if isNull s then String.Empty else s
