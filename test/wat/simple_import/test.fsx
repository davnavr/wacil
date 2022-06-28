#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/simple_import.dll"

open Swensen.Unquote

let imported (a: int64) = a // This is not factorial
let math = simple__import.math(simple__import.math.factorial(imported))
let instance = simple__import(math)

test <@ instance.callImportedFunction() = imported 10L @>
