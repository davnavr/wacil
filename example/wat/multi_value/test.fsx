#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/multi_value.dll"

open Swensen.Unquote

do
    let instance = multi_value.multi_value()
    test <@ instance.doTheThing(5) = struct(5678, 5) @>
    test <@ instance.addThreeThings() = 3 @>
