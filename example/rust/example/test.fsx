#r "nuget: Expecto"
#r "nuget: Expecto.FsCheck"
#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/example.dll"

open Expecto

open FsCheck

open Swensen.Unquote

let addToSelf n = n + n

let instance = example.example()

testList "math" [
    testProperty "implementation is correct" <| fun n ->
        test <@ instance.do_something n = addToSelf n @>
]
|> runTestsWithCLIArgs List.empty Array.empty
|> exit
