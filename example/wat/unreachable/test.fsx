#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/unreachable.dll"

open Swensen.Unquote

let instance = unreachable()

test <@
    let ex =
        try
            instance.doTheThing()
            null
        with
        | ex -> ex

    ex.GetType() = typeof<Wacil.Runtime.UnreachableException>
@>
