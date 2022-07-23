/// Translates WebAssembly instructions into Common Intermediate Language bytecode.
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.Transpiler

open Wacil.Compiler
open Wacil.Compiler.Helpers.Collections

open Wacil.Compiler.Wasm.Format
open Wacil.Compiler.Wasm.Validation.Table

open AsmResolver
open AsmResolver.PE.DotNet.Metadata.Tables.Rows;

open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types

open AsmResolver.PE.DotNet.Cil
open AsmResolver.DotNet.Code.Cil

[<NoComparison; NoEquality>]
type Input = { Expression: ValidExpression; Body: CilMethodBody }

[<RequireQualifiedAccess; NoComparison; ReferenceEquality>]
type private BranchTarget =
    | Loop of CilInstructionLabel
    | Block of CilInstructionLabel
    | If of elseBranchLabel: CilInstructionLabel * endBranchLabel: CilInstructionLabel

[<System.Runtime.CompilerServices.IsByRefLike; Struct; NoComparison; NoEquality>]
type private BranchTargetStack =
    val mutable private targets: ArrayBuilder<BranchTarget>

    new(targets: ArrayBuilder<BranchTarget>) = { targets = targets }

    member this.PushBlock() =
        this.targets.Add(BranchTarget.Block(CilInstructionLabel()))

    member this.PushLoop start =
        this.targets.Add(BranchTarget.Loop(CilInstructionLabel start))

    member this.PushIf(): CilInstructionLabel =
        let elseBranchTarget = CilInstructionLabel()
        this.targets.Add(BranchTarget.If(elseBranchTarget, CilInstructionLabel()))
        elseBranchTarget

    member this.Pop() =
        let target = this.targets.Pop()
        target

    member this.Reset() = this.targets.Clear()

    member this.GetLabel (LabelIdx target): CilInstructionLabel =
        match this.targets.ItemFromEnd target with
        | BranchTarget.Block label
        | BranchTarget.Loop label
        | BranchTarget.If(_, label) -> label

let includeMethodInput (definition: AsmResolver.DotNet.MethodDefinition) expression (inputs: ResizeArray<Input>) =
    definition.CilMethodBody <- CilMethodBody definition
    inputs.Add { Expression = expression; Body = definition.CilMethodBody }

let translateWebAssembly
    (rtlib: RuntimeLibrary.References)
    (members: ModuleMembers)
    (inputs: ResizeArray<Input>)
    =
    let mutable branchTargetStack = BranchTargetStack(ArrayBuilder.Create())

    for { Expression = expression; Body = cil } in inputs do
        let wasm = expression.Instructions
        let il = cil.Instructions

        // Note that WASM functions implicitly introduce a block
        branchTargetStack.PushBlock()

        for i = 0 to wasm.Length - 1 do
            match wasm[i].Instruction with
            | Unreachable ->
                il.Add(CilInstruction(CilOpCodes.Newobj, rtlib.UnreachableExceptionConstructor))
                il.Add(CilInstruction CilOpCodes.Throw)
            | Nop -> il.Add(CilInstruction CilOpCodes.Nop)
            | bad -> raise(System.NotImplementedException(sprintf "Add translation implementation for %A" bad))

            // TODO: Properly handle implicit returns (maybe assume all items are there)
            // TODO: Handle implicit multi-return
            if wasm.Length >= 2 && true (*there is a return somewhere back there*) then
                il.Add(CilInstruction CilOpCodes.Ret)
        ()
