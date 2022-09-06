#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/simd_sample.dll"

open Swensen.Unquote

let instantiation = simd_sample.simd_sample()

test <@ instantiation.getFourIntegers().GetInt32 0 = 1 @>
test <@ instantiation.getFourIntegers().GetInt32 1 = 2 @>

test <@ instantiation.addFourIntegers().GetInt32 2 = 4 @>
test <@ instantiation.addFourIntegers().GetInt32 3 = 5 @>
