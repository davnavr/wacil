#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/my_stack.dll"

open Swensen.Unquote

open Wacil.Runtime

let instance = my__stack()
let stack = uint32(instance._stack_push 4)
Memory.WriteInt32(stack, 9, instance.memory, 0u, 2uy)
Memory.WriteInt32(stack, 10, instance.memory, 4u, 2uy)

test <@ 9 + 10 = instance.addition() @>
