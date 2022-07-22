module Wacil.Compiler.Helpers.Unsafe

open System.Collections.Immutable
open System.Runtime.CompilerServices

let inline isReferenceNull<'T when 'T : not struct> (o: 'T) = System.Object.ReferenceEquals(o, null)

module Array =
    let toImmutable array = Unsafe.As<'a[], ImmutableArray<'a>>(&Unsafe.AsRef(&array))
