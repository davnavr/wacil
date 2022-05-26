[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Helpers.Span

open Microsoft.FSharp.NativeInterop

#nowarn "9" // Uses of this construct may result in the generation of unverifiable .NET IL code.

/// Allocates space for an object on the stack.
let inline stackalloc length =
    // Safety: As this function is always inlined, the pointer returned by the stack allocation is valid for the lifetime of the
    // caller.
    System.Span<'a>(NativePtr.toVoidPtr(NativePtr.stackalloc<'a> length), length)

let inline ofByRef<'a> (reference: byref<'a>) =
    System.Runtime.InteropServices.MemoryMarshal.CreateSpan<'a>(&reference, 1)
