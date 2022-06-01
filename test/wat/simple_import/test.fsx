#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/simple_import.dll"

open Swensen.Unquote

let imported (a: int64) = a // This is not factorial
let math = simple_import.math(simple_import.math.factorial(imported))
let instance = simple_import(math)

test <@ instance.callImportedFunction() = imported 10L @>
