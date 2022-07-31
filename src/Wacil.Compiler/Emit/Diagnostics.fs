module Wacil.Compiler.Emit.Diagnostics

open System.IO

open AsmResolver.DotNet.Code.Cil

let handleInvalidCil action (output: TextWriter) =
    try
        action()
    with
    | :? System.AggregateException as ex ->
        if output <> null then
            for inner in ex.InnerExceptions do
                match inner with
                | :? StackImbalanceException as imbalance ->
                    output.WriteLine(imbalance.Message)
                    for i in imbalance.Body.Instructions do output.WriteLine i
                    output.WriteLine()
                | _ -> ()
        reraise()
