module Wacil.Compiler.Helpers.Unsafe

open System.Collections.Immutable
open System.Runtime.CompilerServices

module Array =
    let toImmutable array = Unsafe.As<'a[], ImmutableArray<'a>>(&Unsafe.AsRef(&array))
