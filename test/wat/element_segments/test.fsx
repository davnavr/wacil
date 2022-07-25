#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/element_segments.dll"

open Swensen.Unquote

do
    let instance = element_segments.element_segments()

    test <@ instance.CallTheFunction() = 1234 + 42 @>

    instance.UseIdentityFunction()

    test <@ instance.CallTheFunction() = 1234 @>
