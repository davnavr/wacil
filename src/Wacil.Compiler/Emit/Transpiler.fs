/// Translates WebAssembly instructions into Common Intermediate Language bytecode.
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.Transpiler

open System.Runtime.CompilerServices

open Wacil.Compiler.Helpers.Collections

open Wacil.Compiler.Wasm.Format
open Wacil.Compiler.Wasm.Validation.Table

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

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type private LocalIndex =
    | Arg of argument: uint16
    | Loc of local: uint16

[<IsByRefLike; Struct; NoComparison; NoEquality>]
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

    member this.Last() = this.targets.ItemFromEnd 0

    member this.GetLabel (LabelIdx target): CilInstructionLabel =
        match this.targets.ItemFromEnd target with
        | BranchTarget.Block label
        | BranchTarget.Loop label
        | BranchTarget.If(_, label) -> label

let includeMethodInput (definition: AsmResolver.DotNet.MethodDefinition) expression (inputs: ResizeArray<Input>) =
    definition.CilMethodBody <- CilMethodBody definition
    inputs.Add { Expression = expression; Body = definition.CilMethodBody }

let private emitComplexComparison comparison (il: CilInstructionCollection) =
    let trueBranchLabel = CilInstructionLabel()
    let endBranchLabel = CilInstructionLabel()
    il.Add(CilInstruction(comparison, trueBranchLabel))
    il.Add(CilInstruction CilOpCodes.Ldc_I4_0)
    il.Add(CilInstruction(CilOpCodes.Br_S, trueBranchLabel))
    trueBranchLabel.Instruction <- CilInstruction CilOpCodes.Ldc_I4_1
    endBranchLabel.Instruction <- CilInstruction CilOpCodes.Nop
    il.Add trueBranchLabel.Instruction
    il.Add endBranchLabel.Instruction

let translateWebAssembly
    (translateValType: _ -> TypeSignature)
    (rtlib: RuntimeLibrary.References)
    (members: ModuleMembers)
    (inputs: ResizeArray<Input>)
    =
    let mutable branchTargetStack = BranchTargetStack(ArrayBuilder.Create())

    let inline (|GlobalIndex|) (GlobalIdx index) = members.Globals[index]

    let emitPushMemory (MemIdx index) (il: CilInstructionCollection) =
        il.Add(CilInstruction CilOpCodes.Ldarg_0)

        match members.Memories[index] with
        | MemoryMember.Defined memory ->
            il.Add(CilInstruction(CilOpCodes.Ldfld, memory))
        | MemoryMember.Imported(import, memory) ->
            il.Add(CilInstruction(CilOpCodes.Ldfld, import))
            il.Add(CilInstruction(CilOpCodes.Ldfld, memory))

    let emitPushMemArg (arg: MemArg) il =
        emitPushMemory arg.Memory il
        il.Add(CilInstruction.CreateLdcI4(int32 arg.Offset))
        il.Add(CilInstruction.CreateLdcI4(int32 arg.Alignment.Power))

    for { Expression = expression; Body = cil } in inputs do
        let wasm = expression.Instructions
        let il = cil.Instructions

        for ty in expression.LocalTypes do
            cil.LocalVariables.Add(CilLocalVariable(translateValType ty))

        branchTargetStack.Reset()
        branchTargetStack.PushBlock() // Note that WASM functions implicitly introduce a block

        let (|LocalIndex|) (LocalIdx index) =
            if index < expression.ParameterTypes.Length then
                // Increment offsets by one, as local index 0 refers to `this`
                Arg(Checked.uint16(index + 1))
            else
                Loc(Checked.uint16(index - expression.ParameterTypes.Length))

        for i = 0 to wasm.Length - 1 do
            match wasm[i].Instruction with
            | Unreachable ->
                il.Add(CilInstruction(CilOpCodes.Newobj, rtlib.UnreachableExceptionConstructor))
                il.Add(CilInstruction CilOpCodes.Throw)
            | Nop -> il.Add(CilInstruction CilOpCodes.Nop)
            | Br target -> il.Add(CilInstruction(CilOpCodes.Br, branchTargetStack.GetLabel target))
            | BrIf target -> il.Add(CilInstruction(CilOpCodes.Brtrue, branchTargetStack.GetLabel target))
            | Return ->
                // TODO: Return will need to be changed when multiple return values are involved (make helper that is used here and in the implicit return added later)
                il.Add(CilInstruction CilOpCodes.Ret)
            | Block _ -> branchTargetStack.PushBlock()
            | Loop _ ->
                let start = CilInstruction CilOpCodes.Nop
                il.Add start
                branchTargetStack.PushLoop start
            | If _ -> il.Add(CilInstruction(CilOpCodes.Brfalse, branchTargetStack.PushIf()))
            | Else ->
                match branchTargetStack.Last() with
                | BranchTarget.If(elseBranchLabel, endBranchLabel) ->
                    il.Add(CilInstruction(CilOpCodes.Br, endBranchLabel))
                    elseBranchLabel.Instruction <- CilInstruction CilOpCodes.Nop
                    il.Add elseBranchLabel.Instruction
                | _ -> invalidOp "unpaired else instruction"
            | End ->
                match branchTargetStack.Pop() with
                | BranchTarget.Loop start -> assert(start.Instruction <> null)
                | BranchTarget.Block label ->
                    label.Instruction <- CilInstruction CilOpCodes.Nop
                    il.Add label.Instruction
                | BranchTarget.If(elseBranchLabel, endBranchLabel) ->
                    endBranchLabel.Instruction <- CilInstruction CilOpCodes.Nop
                    il.Add endBranchLabel.Instruction
                    if isNull elseBranchLabel.Instruction then elseBranchLabel.Instruction <- endBranchLabel.Instruction
            | Call(FuncIdx callee) ->
                // TODO: Reduce code duplication (may when opting to generate a direct call, as it needs to be handled differently for imports and definitions)
                match members.Functions[callee] with
                | FunctionMember.Defined(_, indirect, originalFunctionType) ->
                    // TODO: Handle calling a multi-return function (need to insert a byref to a local here)
                    if originalFunctionType.Results.Length > 1 then
                        failwith "TODO: Handle calling a multi-return function definition"

                    // Parameters are already on the stack in the correct order, so "this" pointer needs to be inserted last
                    il.Add(CilInstruction CilOpCodes.Ldarg_0)
                    il.Add(CilInstruction(CilOpCodes.Call, indirect))
                | FunctionMember.Imported(_, _, _, indirect, originalFunctionType) ->
                    // TODO: Handle calling a multi-return function (need to insert a byref to a local here)
                    if originalFunctionType.Results.Length > 1 then
                        failwith "TODO: Handle calling a multi-return function import"

                    // Parameters are already on the stack in the correct order, so "this" pointer needs to be inserted last
                    il.Add(CilInstruction CilOpCodes.Ldarg_0)
                    il.Add(CilInstruction(CilOpCodes.Call, indirect))
            //| CallIndirect
            | Drop -> il.Add(CilInstruction CilOpCodes.Pop)
            | LocalGet(LocalIndex index) ->
                match index with
                | Arg i -> il.Add(CilInstruction.CreateLdarg i)
                | Loc i -> il.Add(CilInstruction.CreateLdloc i)
            | LocalSet(LocalIndex index) ->
                match index with
                | Arg i -> il.Add(CilInstruction.CreateStarg i)
                | Loc i -> il.Add(CilInstruction.CreateStloc i)
            | GlobalGet(GlobalIndex glbl) ->
                il.Add(CilInstruction CilOpCodes.Ldarg_0)
                match glbl with
                | GlobalMember.Defined(field, _) ->
                    il.Add(CilInstruction(CilOpCodes.Ldfld, field))
                | GlobalMember.DefinedExport(field, accessor, _) ->
                    il.Add(CilInstruction(CilOpCodes.Ldfld, field))
                    il.Add(CilInstruction(CilOpCodes.Call, accessor))
                | GlobalMember.Imported(import, field, accessor, _) ->
                    il.Add(CilInstruction(CilOpCodes.Ldfld, import))
                    il.Add(CilInstruction(CilOpCodes.Ldfld, field))
                    il.Add(CilInstruction(CilOpCodes.Call, accessor))
            | GlobalSet(GlobalIndex glbl) ->
                il.Add(CilInstruction CilOpCodes.Ldarg_0)
                match glbl with
                | GlobalMember.Defined(_, ValueSome(setter)) ->
                    il.Add(CilInstruction(CilOpCodes.Call, setter))
                | GlobalMember.DefinedExport(field, _, setter) ->
                    il.Add(CilInstruction(CilOpCodes.Ldfld, field))
                    il.Add(CilInstruction(CilOpCodes.Call, setter))
                | GlobalMember.Imported(import, field, _, setter) ->
                    il.Add(CilInstruction(CilOpCodes.Ldfld, import))
                    il.Add(CilInstruction(CilOpCodes.Ldfld, field))
                    il.Add(CilInstruction(CilOpCodes.Call, setter))
                | GlobalMember.Defined(_, ValueNone) ->
                    invalidOp "attempt to generate code to mutate constant global variable"
            | I32Load arg ->
                // Top of stack is address to load, which is first parameter
                emitPushMemArg arg il
                il.Add(CilInstruction(CilOpCodes.Call, rtlib.Memory.ReadInt32))
            | I32Store arg ->
                // Stack contains the value to store on top of the address
                emitPushMemArg arg il
                il.Add(CilInstruction(CilOpCodes.Call, rtlib.Memory.WriteInt32))
            | MemoryGrow memory ->
                // Top of the stack is the size delta
                emitPushMemory memory il
                il.Add(CilInstruction(CilOpCodes.Call, rtlib.Memory.Grow))
            | I32Const value -> il.Add(CilInstruction.CreateLdcI4 value)
            | I64Const value ->
                if (value >>> 32) &&& 0xFFFF_FFFFL = 0L then
                    il.Add(CilInstruction.CreateLdcI4(int32 value))
                    il.Add(CilInstruction CilOpCodes.Conv_I8)
                else
                    il.Add(CilInstruction(CilOpCodes.Ldc_I8, value))
            | F32Const value -> il.Add(CilInstruction(CilOpCodes.Ldc_R4, value))
            | F64Const value -> il.Add(CilInstruction(CilOpCodes.Ldc_R8, value))
            | I32Eqz ->
                il.Add(CilInstruction CilOpCodes.Ldc_I4_0)
                il.Add(CilInstruction CilOpCodes.Ceq)
            | I64Eqz ->
                il.Add(CilInstruction CilOpCodes.Ldc_I4_0)
                il.Add(CilInstruction CilOpCodes.Conv_I8)
                il.Add(CilInstruction CilOpCodes.Ceq)
            | I32Eq | I64Eq -> il.Add(CilInstruction CilOpCodes.Ceq)
            | I32Ne | I64Ne ->
                il.Add(CilInstruction CilOpCodes.Ceq)
                il.Add(CilInstruction CilOpCodes.Ldc_I4_0)
                il.Add(CilInstruction CilOpCodes.Ceq)
            // TODO: Make sure that the direction of the comparison operation is actually correct
            | I32LtS | I64LtS -> il.Add(CilInstruction CilOpCodes.Clt)
            | I32LtU | I64LtU -> il.Add(CilInstruction CilOpCodes.Clt_Un)
            | I32GtS | I64GtS -> il.Add(CilInstruction CilOpCodes.Cgt)
            | I32GtU | I64GtU -> il.Add(CilInstruction CilOpCodes.Cgt_Un)
            | I32LeS | I64LeS -> emitComplexComparison CilOpCodes.Ble_S il
            | I32LeU | I64LeU -> emitComplexComparison CilOpCodes.Ble_Un_S il
            | I32GeS | I64GeS -> emitComplexComparison CilOpCodes.Bge_S il
            | I32GeU | I64GeU -> emitComplexComparison CilOpCodes.Bge_Un_S il
            | I32Add | I64Add -> il.Add(CilInstruction CilOpCodes.Add)
            | I32Sub | I64Sub -> il.Add(CilInstruction CilOpCodes.Sub)
            | I32Mul | I64Mul -> il.Add(CilInstruction CilOpCodes.Mul)
            | I32DivS | I64DivS -> il.Add(CilInstruction CilOpCodes.Div)
            | I32DivU | I64DivU -> il.Add(CilInstruction CilOpCodes.Div_Un)
            | I32And -> il.Add(CilInstruction CilOpCodes.And)
            | I32Or -> il.Add(CilInstruction CilOpCodes.Or)
            | I32Xor -> il.Add(CilInstruction CilOpCodes.Xor)
            | bad -> raise(System.NotImplementedException(sprintf "Add translation implementation for %A" bad))

        // TODO: TODO TODO: Move implicit return code to the handler for End, End handler could just reuse code for Ret
        // TODO: Properly handle implicit returns (maybe assume all items are there)
        // TODO: Handle implicit multi-return
        if wasm.Length >= 2 && true (*there is a return somewhere back there*) then
            il.Add(CilInstruction CilOpCodes.Ret)
