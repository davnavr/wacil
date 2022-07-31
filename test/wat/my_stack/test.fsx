#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/my_stack.dll"

open Swensen.Unquote

open Wacil.Runtime

let memory = ArrayMemory(Limits(1)) :> IMemory32
let instance = my_stack.my_stack(my_stack.env memory)

let first = instance._stack_push 4
memory.Write(first, 2uy, 9)

let second = instance._stack_push 4
memory.Write(second, 2uy, 10)

test <@ 9 + 10 = instance.addition() @>
