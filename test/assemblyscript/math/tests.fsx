#r "nuget: Expecto"
#r "nuget: Expecto.FsCheck"
#r "nuget: Unquote"
#r "./out/math.dll"

open Expecto

open FsCheck

open Swensen.Unquote

let factorial n =
    let rec inner i acc =
        match i with
        | 0L
        | 1L -> acc
        | _ -> inner (i - 1L) (acc * i)
    inner n 1L

testList "math" [
    testCase "favorite number is correct" <| fun _ ->
        math.my_favorite_integer =! 19

    testProperty "even numbers are correctly classified" <| fun (PositiveInt n) ->
        test <@ (math.is_even n = 1) = (n % 2 = 0) @>

    testProperty "factorial implementation is correct" <| fun (PositiveInt i) ->
        let n = int64 i
        test <@ math.factorial n = factorial n @>
]
|> runTestsWithCLIArgs List.empty Array.empty
|> exit
