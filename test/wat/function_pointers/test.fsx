#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/function_pointers.dll"

open Swensen.Unquote

let instance = function_pointers()
instance.functions[0] <- System.Action<int>(printfn "I got %i")

// type Thing = delegate of int -> unit
// let a = instance.functions[0]
// let b = System.Delegate.CreateDelegate(typeof<Thing>, a.Target, a.Method) :?> Thing
// b.Invoke 5
