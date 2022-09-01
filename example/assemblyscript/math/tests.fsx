#r "nuget: Expecto"
#r "nuget: Expecto.FsCheck"
#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/math.dll"

open Expecto

open FsCheck

open Swensen.Unquote

let abort _ _ _ _ = failwith "ABORTED"

let factorial n =
    let rec inner i acc =
        match i with
        | 0L
        | 1L -> acc
        | _ -> inner (i - 1L) (acc * i)
    inner n 1L

let quadratic (a: double) b c x: double =
    (a * x * x) + (b * x) + c

let instance = math.math(math.env(abort))

testList "math" [
    testCase "favorite number is correct" <| fun _ ->
        instance.my_favorite_integer.Value =! 19

    testProperty "even numbers are correctly classified" <| fun (PositiveInt n) ->
        test <@ (instance.is_even n = 1) = (n % 2 = 0) @>

    testProperty "factorial implementation is correct" <| fun (PositiveInt i) ->
        let n = int64 i
        test <@ instance.factorial n = factorial n @>

    testProperty "quadratic implementation is correct" <| fun (n: int) ->
        let x = double n
        test <@ quadratic 4.0 3.0 2.0 x = instance.quadratic(4, 3, 2, x) @>
]
|> runTestsWithCLIArgs List.empty Array.empty
|> exit
