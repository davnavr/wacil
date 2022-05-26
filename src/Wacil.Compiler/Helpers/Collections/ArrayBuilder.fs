namespace Wacil.Compiler.Helpers.Collections

open System.Runtime.CompilerServices

/// Helper class to create collections based on arrays.
[<IsByRefLike; Struct; NoComparison; NoEquality>]
type internal ArrayBuilder<'a> =
    val mutable buffer: 'a[]
    val mutable length: int

    private new(buffer: 'a[]) = { buffer = buffer; length = 0 }

    static member Create(capacity: int32) =
        if capacity < 0 then raise(System.ArgumentOutOfRangeException(nameof capacity))
        ArrayBuilder(if capacity = 0 then System.Array.Empty() else Array.zeroCreate capacity)

    static member Create() = ArrayBuilder<'a>.Create(0)

    member this.Capacity = this.buffer.Length

    member this.Length = this.length

    member private this.EnsureCapacity() =
        if this.length > this.buffer.Length then
            let newBufferCapacity =
                if this.buffer.Length < 4 then
                    4
                else
                    Checked.uint32 this.length
                    |> System.Numerics.BitOperations.RoundUpToPowerOf2
                    |> Checked.int

            System.Array.Resize(&this.buffer, newBufferCapacity)

    member this.Add(item: 'a) =
        let index = this.length
        this.length <- index + 1
        this.EnsureCapacity()
        this.buffer[index] <- item

    member this.ToArray() =
        if this.Capacity = this.length then
            let array = this.buffer
            this.buffer <- System.Array.Empty()
            this.length <- 0
            array
        else
            let array = Array.zeroCreate this.length
            System.Span(this.buffer).CopyTo(System.Span(array))
            array

    member this.ToImmutableArray() =
        Wacil.Compiler.Helpers.Unsafe.Array.toImmutable(this.ToArray())