namespace Wacil.Compiler.Helpers.Collections

open System.Runtime.CompilerServices

/// A mutable array that can be resized and converted into other collection types.
[<Struct; NoComparison; NoEquality>]
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

    member this.IsEmpty = this.length = 0

    member private this.EnsureCapacity() =
        if this.length > this.buffer.Length then
            let newBufferCapacity =
                if this.length <= 2 then
                    2
                else
                    Checked.uint32 this.length
                    |> System.Numerics.BitOperations.RoundUpToPowerOf2
                    |> Checked.int

            System.Array.Resize(&this.buffer, newBufferCapacity)

    member this.Clear() = this.length <- 0

    member this.Add(item: 'a) =
        let index = this.length
        this.length <- index + 1
        this.EnsureCapacity()
        this.buffer[index] <- item

    member this.TryPop(value: outref<'a>) =
        if this.length > 0 then
            this.length <- this.length - 1
            value <- this.buffer[this.length]
            true
        else
            false

    member this.Pop(value: outref<'a>) =
        if not(this.TryPop(&value)) then invalidOp "Cannot remove item when array is empty"

    /// Returns a mutable reference to the last element of the array.
    member this.LastRef() = &this.buffer[this.length - 1]

    member this.ToArray() =
        if this.Capacity = this.length then
            let array = this.buffer
            this.buffer <- System.Array.Empty()
            this.length <- 0
            array
        else
            let array = Array.zeroCreate this.length
            System.Span(this.buffer, 0, this.length).CopyTo(System.Span(array))
            array

    member this.ToImmutableArray() =
        Wacil.Compiler.Helpers.Unsafe.Array.toImmutable(this.ToArray())
