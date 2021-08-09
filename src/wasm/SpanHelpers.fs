[<RequireQualifiedAccess>]
module internal Wasm.SpanHelpers

open System

open Microsoft.FSharp.NativeInterop

#nowarn "9" // Uses of this construct may result in the generation of unverifiable .NET IL code.

let inline stackalloc length = Span<'T>(NativePtr.toVoidPtr(NativePtr.stackalloc<'T> length), length)

let inline readonly (value: Span<'T>) = Span<'T>.op_Implicit value
