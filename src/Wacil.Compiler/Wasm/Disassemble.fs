module Wacil.Compiler.Wasm.Disassemble

open System.Collections.Generic
open System.IO

open Wacil.Compiler.Wasm.Validation

type Node =
    | Text of string
    | List of ResizeArray<Node>

let rec printNodesToWriter (node: Node) (output: TextWriter) =
    match node with
    | Text text -> output.Write(text)
    | List items ->
        output.Write('(')
        for i = 0 to items.Count - 1 do
            if i > 0 then output.Write(' ')
            printNodesToWriter items[i] output
        output.Write(')')

let indexTextNode i = Text(sprintf "(;%i;)" i)

let disassemble (input: ValidModule) =
    let topLevelNodes = ResizeArray()
    topLevelNodes.Add(Text "module")

    for i = 0 to input.Functions.Length - 1 do
        let func = input.Functions[i]
        let functionNode = ResizeArray()
        functionNode.Add(Text "func")
        functionNode.Add(indexTextNode i)
        topLevelNodes.Add(List functionNode)

    List topLevelNodes

let disassembleToWriter (input: ValidModule) (output: TextWriter) =
    let node = disassemble input
    printNodesToWriter node output
    output.Flush()
