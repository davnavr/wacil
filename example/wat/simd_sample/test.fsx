#r "nuget: Unquote"
#r "./out/Wacil.Runtime.dll"
#r "./out/simd_sample.dll"

open Swensen.Unquote

let instantiation = simd_sample.simd_sample()

test <@ instantiation.getFourIntegers().GetInt32 0 = 1 @>
