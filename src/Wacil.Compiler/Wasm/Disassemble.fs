module Wacil.Compiler.Wasm.Disassemble

open System.Collections.Immutable
open System.IO

open Wacil.Compiler.Wasm.Format
open Wacil.Compiler.Wasm.Validation

[<Sealed>]
type Writer (output: TextWriter) =
    let mutable indentation = 0
    let mutable indented = false

    member _.Indent() = indentation <- Checked.(+) indentation 1
    member _.Dedent() = indentation <- Checked.(-) indentation 1

    member private _.WriteIndentation() =
        if not indented then
            for _ = 0 to indentation - 1 do output.Write("  ")
            indented <- true

    member this.Write(c: char) =
        this.WriteIndentation()
        output.Write(c)

    member this.Write(s: string) =
        this.WriteIndentation()
        output.Write(s)

    member this.WriteLine() =
        this.WriteIndentation()
        output.WriteLine()
        indented <- false

    member this.WriteLine(c: char) =
        this.WriteIndentation()
        output.WriteLine(c)
        indented <- false

    member this.WriteLine(s: string) =
        this.WriteIndentation()
        output.WriteLine(s)
        indented <- false

    member this.WriteIndexComment(index: int32) = this.WriteLine(sprintf "(;%i;)" index)

    interface System.IDisposable with
        member _.Dispose() = output.Dispose()

let disassembleInstructionSequence (instructions: ImmutableArray<Instruction>) (out: Writer) =
    for instruction in instructions do
        raise(System.NotImplementedException())

        out.WriteLine()

let disassembleToWriter (input: ValidModule) (output: TextWriter) =
    use out = new Writer(output)
    out.WriteLine "(module"
    out.Indent()
    for i = 0 to input.Functions.Length - 1 do
        let func = input.Functions[i]
        out.Write "(func "
        out.WriteIndexComment i
        // TODO: Write function types
        out.WriteLine()
        out.Indent()
        // TODO: Write locals
        //disassembleInstructionSequence func.Body.Instructions out
        out.WriteLine ')'
        out.Dedent()
    out.WriteLine ')'
