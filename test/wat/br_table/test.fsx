#r "nuget: Expecto"
#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/br_table.dll"

open Expecto
open Swensen.Unquote

let nthTriangleNumber n =
    let rec inner n acc =
        match n with
        | 0u -> acc
        | _ -> inner (n - 1u) (acc + uint64 n)
    inner n 0UL

let instance = br_table.br_table()

List.init 100 (fun n -> testCase (string n) (fun _ -> test <@ nthTriangleNumber(uint32 n) = uint64(instance.nthTriangleNumber n) @>))
|> testList "triangle numbers"
|> runTestsWithCLIArgs List.empty Array.empty
|> exit
