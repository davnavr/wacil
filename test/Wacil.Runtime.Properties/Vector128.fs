module Wacil.Runtime.Properties.Vector128

open Wacil.Runtime

open Expecto
open Swensen.Unquote

[<Tests>]
let tests = testList "v128" [
    testProperty "integer addition is correct" <| fun (a: int) b c d (e: int) f g h ->
        test <@
            let x = Vector128(a, b, c, d)
            let y = Vector128(e, f, g, h)
            let result = x.AddInt32 y
            a + e = result.GetInt32 0 && b + f = result.GetInt32 1 && c + g = result.GetInt32 2 && d + h = result.GetInt32 3
        @>
]
