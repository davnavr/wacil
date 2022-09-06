module Wacil.Runtime.Properties.Vector128

open Wacil.Runtime

open Expecto
open Swensen.Unquote

[<Tests>]
let tests = testList "v128" [
    testProperty "i32 addition is correct" <| fun (a: int) b c d (e: int) f g h ->
        test <@
            let x = Vector128(a, b, c, d)
            let y = Vector128(e, f, g, h)
            let result = x.AddInt32 y
            a + e = result.GetInt32 0 && b + f = result.GetInt32 1 && c + g = result.GetInt32 2 && d + h = result.GetInt32 3
        @>

    testProperty "i16 addition is correct" <| fun (a1: int16) b1 c1 d1 e1 f1 g1 h1 (a2: int16) b2 c2 d2 e2 f2 g2 h2 ->
        test <@
            let x = Vector128(a1, b1, c1, d1, e1, f1, g1, h1)
            let y = Vector128(a2, b2, c2, d2, e2, f2, g2, h2)
            let result = x.AddInt16 y
            a1 + a2 = result.GetInt16 0 &&
            b1 + b2 = result.GetInt16 1 &&
            c1 + c2 = result.GetInt16 2 &&
            d1 + d2 = result.GetInt16 3 &&
            e1 + e2 = result.GetInt16 4 &&
            f1 + f2 = result.GetInt16 5 &&
            g1 + g2 = result.GetInt16 6 &&
            h1 + h2 = result.GetInt16 7
        @>
]
