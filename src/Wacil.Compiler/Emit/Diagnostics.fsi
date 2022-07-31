[<RequireQualifiedAccess>]
module Wacil.Compiler.Emit.Diagnostics

val handleInvalidCil : (unit -> 'T) -> output: System.IO.TextWriter -> 'T
