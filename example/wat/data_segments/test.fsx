#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/data_segments.dll"

open Swensen.Unquote

// necessary since Span cannot be stored in static fields
do
    let instance = data_segments.data_segments()
    let buffer = System.Span<byte>(Array.zeroCreate 4)
    instance.memory.Read(0x200, buffer)
    let bytes = buffer.ToArray()
    test <@ bytes = "Hey?"B @>
