#r "nuget: Unquote"
#r "nuget: Expecto"
#r "nuget: Expecto.FsCheck"
#r "./out/Wacil.Runtime.dll"
#r "./out/multi_value.dll"

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

let instance = multi_value.multi_value()

testList "multi_value" [
    testCase "doTheThing" <| fun _ ->
        test <@ instance.doTheThing(5) = struct(5678, 5) @>

    testCase "addThreeThings" <| fun _ ->
        test <@ instance.addThreeThings() = 3 @>

    // testProperty "factorial implementation is correct" <| fun (PositiveInt i) ->
    //     let n = int64 i
    //     test <@ instance.factorial n = factorial n @>
]
|> runTestsWithCLIArgs List.empty Array.empty
|> exit
