[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Helpers.Unsafe

open System.Collections.Immutable

val inline isReferenceNull<'T when 'T : not struct> : 'T -> bool

module Array =
    /// <summary>Creates an immutable array from a mutable array.</summary>
    /// <remarks>Callers must ensure that there are no references to the original array.</remarks>
    val toImmutable : array: 'a[] -> ImmutableArray<'a>
