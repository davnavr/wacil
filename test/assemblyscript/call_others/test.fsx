#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/call_others.dll"

open Swensen.Unquote

let instance = call__others()

test <@ instance.doSomething 5 = 59 @>
