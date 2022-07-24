#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/function_pointers.dll"

open Wacil.Runtime

open Swensen.Unquote

do
    let instance = function_pointers.function_pointers(function_pointers.env(fun _ _ _ _ -> failwith "ABORT"))
    
    Table.Grow(null, 1, instance.table)
    instance.table[0] <- System.Func<int64, int64>(fun a -> a + 5L) :> System.MulticastDelegate
    
    test <@ instance.callFunctionPointer 5 = 10 @>
