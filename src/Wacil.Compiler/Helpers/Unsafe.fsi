[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Helpers.Unsafe

open System.Collections.Immutable

module Array =
    /// Creates an immutable array from a mutable array. Callers must ensure that there are no references to the original array.
    val toImmutable : array: 'a[] -> ImmutableArray<'a>
