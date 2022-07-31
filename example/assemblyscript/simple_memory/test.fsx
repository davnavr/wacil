#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/simple_memory.dll"

open Swensen.Unquote

let instance = simple_memory()

test <@ instance.memory <> null @>
test <@ instance.getFunnyNumber() = 42 @>
