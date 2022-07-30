#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/select.dll"

open Swensen.Unquote

let instantiation = select.select()

test <@ instantiation.MyFunction(7) = 26 @>
test <@ instantiation.MyFunction(8) = 62 @>
