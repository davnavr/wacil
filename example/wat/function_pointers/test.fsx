#r "./out/Wacil.Runtime.dll"
#r "./out/function_pointers.dll"

let instance = function_pointers.function_pointers()
instance.Functions[0] <- System.Action<int>(printfn "I got %i")
instance.DoIt()
