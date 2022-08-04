/// Translates WebAssembly instructions into Common Intermediate Language bytecode.
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.Transpiler

open System.Collections.Immutable
open System.Runtime.CompilerServices

open Wacil.Compiler.Helpers.Collections

open Wacil.Compiler.Wasm.Format
open Wacil.Compiler.Wasm.Validation.Table

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

    member this.Depth = this.targets.Length

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
    il.Add(CilInstruction(CilOpCodes.Br_S, endBranchLabel))
    trueBranchLabel.Instruction <- CilInstruction CilOpCodes.Ldc_I4_1
    endBranchLabel.Instruction <- CilInstruction CilOpCodes.Nop
    il.Add trueBranchLabel.Instruction
    il.Add endBranchLabel.Instruction

let translateWebAssembly
    (translateValType: ValType -> TypeSignature)
    (translateFuncType: FuncType -> MethodSignature)
    (tupleTypeCache: _ -> TupleCache.Instantiation)
    (delegateTypeCache: MethodSignature -> DelegateCache.Instantiation)
    (syslib: SystemLibrary.References)
    (rtlib: RuntimeLibrary.References)
    (wasm: Wacil.Compiler.Wasm.Validation.ValidModule)
    (members: ModuleMembers)
    (inputs: ResizeArray<Input>)
    =
    let mutable branchTargetStack = BranchTargetStack(ArrayBuilder.Create())

    let temporaryLocalCacheFactory, clearTemporaryLocalCache =
        let lookup = System.Collections.Generic.Dictionary<TypeSignature, uint16>()

        let cache (body: CilMethodBody) ty =
            match lookup.TryGetValue ty with
            | true, existing -> existing
            | false, _ ->
                let index = Checked.uint16 body.LocalVariables.Count
                body.LocalVariables.Add(CilLocalVariable ty)
                lookup[ty] <- index
                index

        cache, fun() -> lookup.Clear()

    let inline (|TypeIndex|) (TypeIdx index) = wasm.Types[index]
    let inline (|GlobalIndex|) (GlobalIdx index) = members.Globals[index]
    let inline (|ElementIndex|) (ElemIdx index) = members.ElementSegments[index]

    let emitPushMemory (MemIdx index) (il: CilInstructionCollection) =
        il.Add(CilInstruction CilOpCodes.Ldarg_0)

        match members.Memories[index] with
        | MemoryMember.Defined(memory, instantiation) ->
            il.Add(CilInstruction(CilOpCodes.Ldfld, memory))
            instantiation
        | MemoryMember.Imported(import, memory, instantiation) ->
            il.Add(CilInstruction(CilOpCodes.Ldfld, import))
            il.Add(CilInstruction(CilOpCodes.Ldfld, memory))
            instantiation

    let emitPushTable (TableIdx index) (il: CilInstructionCollection) =
        il.Add(CilInstruction CilOpCodes.Ldarg_0)
        match members.Tables[index] with
        | TableMember.Defined(table, instantiation) ->
            il.Add(CilInstruction(CilOpCodes.Ldfld, table))
            instantiation
        | TableMember.Imported(import, table, instantiation) ->
            il.Add(CilInstruction(CilOpCodes.Ldfld, import))
            il.Add(CilInstruction(CilOpCodes.Ldfld, table))
            instantiation

    let emitPushMemArg (arg: MemArg) il =
        let instantiation = emitPushMemory arg.Memory il
        il.Add(CilInstruction.CreateLdcI4(int32 arg.Offset))
        il.Add(CilInstruction.CreateLdcI4(int32 arg.Alignment.Power))
        instantiation

    let emitMultiValueDeconstruct (types: ImmutableArray<ValType>) temporaryLocalCache (il: CilInstructionCollection) =
        let tupleTypeInstantiation = tupleTypeCache types

        if tupleTypeInstantiation.Fields.Length <> types.Length then invalidOp "tuple field count mismatch"

        // Assume that the tuple is on top of the stack
        let temporary = temporaryLocalCache tupleTypeInstantiation.Signature
        il.Add(CilInstruction.CreateStloc temporary)

        for field in tupleTypeInstantiation.Fields do
            il.Add(CilInstruction.CreateLdloc temporary)
            il.Add(CilInstruction(CilOpCodes.Ldfld, field))

    let emitFunctionReturn (originalReturnTypes: ImmutableArray<ValType>) (il: CilInstructionCollection) =
        // Assume all return values (if there are any) are on top of the stack
        if originalReturnTypes.Length >= 2 then
            let tupleTypeInstantiation = tupleTypeCache originalReturnTypes
            il.Add(CilInstruction(CilOpCodes.Newobj, tupleTypeInstantiation.Constructor))
        
        il.Add(CilInstruction CilOpCodes.Ret)

    for { Expression = expression; Body = cil } in inputs do
        let wasm = expression.Instructions
        let il = cil.Instructions
        cil.InitializeLocals <- true

        for ty in expression.LocalTypes do
            cil.LocalVariables.Add(CilLocalVariable(translateValType ty))

        branchTargetStack.Reset()
        branchTargetStack.PushBlock() // Note that WASM functions implicitly introduce a block

        let temporaryLocalCache =
            clearTemporaryLocalCache()
            temporaryLocalCacheFactory cil

        let (|LocalIndex|) (LocalIdx index) =
            if index < expression.ParameterTypes.Length then
                // Increment offsets by one, as local index 0 refers to `this`
                Arg(Checked.uint16(index + 1))
            else
                Loc(Checked.uint16(index - expression.ParameterTypes.Length))

        // Note that translation of most instructions works out even with multi-value
        for i = 0 to wasm.Length - 1 do
            let instruction = wasm[i]
            match instruction.Instruction with
            | Unreachable ->
                il.Add(CilInstruction(CilOpCodes.Newobj, rtlib.UnreachableExceptionConstructor))
                il.Add(CilInstruction CilOpCodes.Throw)
            | Nop | DataDrop _ -> il.Add(CilInstruction CilOpCodes.Nop)
            | Br target -> il.Add(CilInstruction(CilOpCodes.Br, branchTargetStack.GetLabel target))
            | BrIf target -> il.Add(CilInstruction(CilOpCodes.Brtrue, branchTargetStack.GetLabel target))
            | BrTable(targetLabels, defaultLabel) ->
                // Integer index is on top of the stack
                let translatedTargetLabels = ResizeArray<ICilLabel> targetLabels.Length
                for target in targetLabels do translatedTargetLabels.Add(branchTargetStack.GetLabel target)
                il.Add(CilInstruction(CilOpCodes.Switch, translatedTargetLabels))
                // Branch to default label
                il.Add(CilInstruction(CilOpCodes.Br, branchTargetStack.GetLabel defaultLabel))
            | Return -> emitFunctionReturn expression.ResultTypes il
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
                    
                    if branchTargetStack.Depth = 0 && not instruction.Unreachable then
                        emitFunctionReturn expression.ResultTypes il
                | BranchTarget.If(elseBranchLabel, endBranchLabel) ->
                    endBranchLabel.Instruction <- CilInstruction CilOpCodes.Nop
                    il.Add endBranchLabel.Instruction
                    if isNull elseBranchLabel.Instruction then elseBranchLabel.Instruction <- endBranchLabel.Instruction
            | Call(FuncIdx callee) ->
                // TODO: Reduce code duplication (may when opting to generate a direct call, as it needs to be handled differently for imports and definitions)
                let originalResultTypes =
                    // Parameters are already on the stack in the correct order, so "this" pointer needs to be inserted last
                    match members.Functions[callee] with
                    | FunctionMember.Defined(_, indirect, originalFunctionType) ->
                        il.Add(CilInstruction CilOpCodes.Ldarg_0)
                        il.Add(CilInstruction(CilOpCodes.Call, indirect))
                        originalFunctionType.Results
                    | FunctionMember.Imported(_, _, _, indirect, originalFunctionType) ->
                        il.Add(CilInstruction CilOpCodes.Ldarg_0)
                        il.Add(CilInstruction(CilOpCodes.Call, indirect))
                        originalFunctionType.Results

                // Return value is now on top of the stack
                if originalResultTypes.Length > 1 then
                    emitMultiValueDeconstruct originalResultTypes temporaryLocalCache il
            | CallIndirect(TypeIndex originalFunctionType, table) ->
                // TODO: If no arguments, can optimize and call Invoke directly instaed of using the InvokeHelper
                let functionTypeInstantiation = delegateTypeCache(translateFuncType originalFunctionType)

                // Parameters are already on the stack in the correct order
                emitPushTable table il |> ignore

                // At this point, the index of the function is on the top of the stack
                il.Add(CilInstruction(CilOpCodes.Call, functionTypeInstantiation.TableGetHelper))

                // At this point, the delegate is on the top of the stack, so invoke helper can be called
                il.Add(CilInstruction(CilOpCodes.Call, functionTypeInstantiation.InvokeHelper))

                // Return value is now on top of the stack
                if originalFunctionType.Results.Length > 1 then
                    emitMultiValueDeconstruct originalFunctionType.Results temporaryLocalCache il
            | Drop -> il.Add(CilInstruction CilOpCodes.Pop)
            | Select ->
                // Types of items to select from are the same, but are not encoded in the instruction
                match instruction.StackState with
                | StackState.Omitted ->
                    if not instruction.Unreachable then failwith "Unable to infer type for select instruction"
                | StackState.PoppedValues types ->
                    let storage = temporaryLocalCache (translateValType types[0])
                    let selectOtherValue = CilInstruction.CreateStloc storage
                    let endOfSelect = CilInstruction CilOpCodes.Nop
                    il.Add(CilInstruction(CilOpCodes.Brfalse_S, CilInstructionLabel selectOtherValue))
                    il.Add(CilInstruction CilOpCodes.Pop) // Selected value is on top of the stack
                    il.Add(CilInstruction(CilOpCodes.Br_S, CilInstructionLabel endOfSelect))
                    il.Add selectOtherValue
                    il.Add(CilInstruction CilOpCodes.Pop)
                    il.Add(CilInstruction.CreateLdloc storage)
                    il.Add endOfSelect
            | LocalGet(LocalIndex index) ->
                match index with
                | Arg i -> il.Add(CilInstruction.CreateLdarg i)
                | Loc i -> il.Add(CilInstruction.CreateLdloc i)
            | LocalSet(LocalIndex index) ->
                match index with
                | Arg i -> il.Add(CilInstruction.CreateStarg i)
                | Loc i -> il.Add(CilInstruction.CreateStloc i)
            | LocalTee(LocalIndex index) ->
                il.Add(CilInstruction CilOpCodes.Dup)
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
            | TableGet table ->
                // Index of the element to retrieve is on the top of the stack.
                let instantiation = emitPushTable table il
                il.Add(CilInstruction(CilOpCodes.Call, instantiation.Get))
            | I32Load arg ->
                // Top of stack is address to load, which is first parameter
                let instantiation = emitPushMemArg arg il
                il.Add(CilInstruction(CilOpCodes.Call, instantiation.ReadInt32))
            | I64Load arg ->
                let instantiation = emitPushMemArg arg il
                il.Add(CilInstruction(CilOpCodes.Call, instantiation.ReadInt64))
            | I32Load8U arg ->
                let instantiation = emitPushMemory arg.Memory il
                il.Add(CilInstruction.CreateLdcI4(int32 arg.Offset))
                il.Add(CilInstruction(CilOpCodes.Call, instantiation.ReadByte))
                il.Add(CilInstruction CilOpCodes.Conv_U4)
            | I32Load8S arg ->
                let instantiation = emitPushMemory arg.Memory il
                il.Add(CilInstruction.CreateLdcI4(int32 arg.Offset))
                il.Add(CilInstruction(CilOpCodes.Call, instantiation.ReadByte))
                il.Add(CilInstruction CilOpCodes.Conv_I4)
            | I32Load16U arg ->
                let instantiation = emitPushMemArg arg il
                il.Add(CilInstruction(CilOpCodes.Call, instantiation.ReadInt16))
                il.Add(CilInstruction CilOpCodes.Conv_U4)
            | I64Load32U arg ->
                let instantiation = emitPushMemArg arg il
                il.Add(CilInstruction(CilOpCodes.Call, instantiation.ReadInt32))
                il.Add(CilInstruction CilOpCodes.Conv_U8)
            | I32Store arg ->
                // Stack contains the value to store on top of the address
                let instantiation = emitPushMemArg arg il
                il.Add(CilInstruction(CilOpCodes.Call, instantiation.WriteInt32))
            | I64Store arg ->
                let instantiation = emitPushMemArg arg il
                il.Add(CilInstruction(CilOpCodes.Call, instantiation.WriteInt64))
            | I32Store8 arg ->
                // TODO: Figure out if an explicit Conv_I1 instruction needs to be emitted.
                let instantiation = emitPushMemory arg.Memory il
                il.Add(CilInstruction.CreateLdcI4(int32 arg.Offset))
                il.Add(CilInstruction(CilOpCodes.Call, instantiation.WriteByte))
            | I32Store16 arg ->
                let instantiation = emitPushMemArg arg il
                il.Add(CilInstruction(CilOpCodes.Call, instantiation.WriteInt16))
            | MemorySize memory ->
                let instantiation = emitPushMemory memory il
                // Memory instance is on top of the stack
                il.Add(CilInstruction(instantiation.ThisCallOpCode, instantiation.PageCount))
            | MemoryGrow memory ->
                // Top of the stack is the size delta
                let instantiation = emitPushMemory memory il
                il.Add(CilInstruction(CilOpCodes.Call, instantiation.Grow))
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
            | I32Rotl ->
                // The integer to rotate is popped last
                il.Add(CilInstruction(CilOpCodes.Call, rtlib.IntegerHelpers.RotateLeftInt32))
            | I32Rotr -> il.Add(CilInstruction(CilOpCodes.Call, rtlib.IntegerHelpers.RotateRightInt32))
            | I32Clz -> il.Add(CilInstruction(CilOpCodes.Call, syslib.BitOperations.LeadingZeroCountUInt32))
            | I32Ctz -> il.Add(CilInstruction(CilOpCodes.Call, syslib.BitOperations.TrailingZeroCountUInt32))
            | I32Add | I64Add -> il.Add(CilInstruction CilOpCodes.Add)
            | I32Sub | I64Sub -> il.Add(CilInstruction CilOpCodes.Sub)
            | I32Mul | I64Mul -> il.Add(CilInstruction CilOpCodes.Mul)
            | I32DivS | I64DivS -> il.Add(CilInstruction CilOpCodes.Div)
            | I32DivU | I64DivU -> il.Add(CilInstruction CilOpCodes.Div_Un)
            | I32RemU -> il.Add(CilInstruction CilOpCodes.Rem_Un)
            | I32And | I64And -> il.Add(CilInstruction CilOpCodes.And)
            | I32Or | I64Or -> il.Add(CilInstruction CilOpCodes.Or)
            | I32Xor | I64Xor -> il.Add(CilInstruction CilOpCodes.Xor)
            | I32Shl | I64Shl -> il.Add(CilInstruction CilOpCodes.Shl)
            | I32ShrU | I64ShrU -> il.Add(CilInstruction CilOpCodes.Shr_Un)
            | I64Rotl -> il.Add(CilInstruction(CilOpCodes.Call, rtlib.IntegerHelpers.RotateLeftInt64))
            | I64Rotr -> il.Add(CilInstruction(CilOpCodes.Call, rtlib.IntegerHelpers.RotateRightInt64))
            | I32WrapI64 -> il.Add(CilInstruction CilOpCodes.Conv_I4)
            | I64ExtendI32U -> il.Add(CilInstruction CilOpCodes.Conv_U8)
            | RefNull _ -> il.Add(CilInstruction CilOpCodes.Ldnull)
            | RefFunc(FuncIdx func) ->
                il.Add(CilInstruction CilOpCodes.Ldarg_0)
                match members.Functions[int32 func] with
                | FunctionMember.Imported(import, field, _, _, _) ->
                    il.Add(CilInstruction(CilOpCodes.Ldfld, import))
                    il.Add(CilInstruction(CilOpCodes.Ldfld, field))
                | FunctionMember.Defined(method, _, _) ->
                    let delegateTypeInstance = delegateTypeCache method.Signature
                    il.Add(CilInstruction(CilOpCodes.Ldftn, method))
                    il.Add(CilInstruction(CilOpCodes.Newobj, delegateTypeInstance.Constructor))
            | MemoryCopy(destination, source) ->
                let sourceMemoryInstantiation = emitPushMemory source il
                let destinationMemoryInstantiation = emitPushMemory destination il
                il.Add(CilInstruction(CilOpCodes.Call, sourceMemoryInstantiation.Copy destinationMemoryInstantiation))
            | MemoryFill memory ->
                let instantiation = emitPushMemory memory il
                il.Add(CilInstruction(CilOpCodes.Call, instantiation.Fill))
            | TableInit(ElementIndex element, table) ->
                match element with
                | ElementSegmentMember.Passive(_, getter) ->
                    // The count, element index, and table index are on top of the stack
                    let tableTypeInstantiation = emitPushTable table il

                    // Table is on top of the stack
                    il.Add(CilInstruction CilOpCodes.Ldarg_0)
                    il.Add(CilInstruction(CilOpCodes.Call, getter))

                    il.Add(CilInstruction(CilOpCodes.Call, tableTypeInstantiation.Initialize))
                | ElementSegmentMember.Active | ElementSegmentMember.Declarative ->
                    invalidOp "Attempt to initialize with element segment that is not passive"
            | ElemDrop(ElementIndex element) ->
                match element with
                | ElementSegmentMember.Passive(field, _) ->
                    il.Add(CilInstruction CilOpCodes.Ldarg_0)
                    il.Add(CilInstruction CilOpCodes.Ldnull)
                    il.Add(CilInstruction(CilOpCodes.Stfld, field))
                | ElementSegmentMember.Active | ElementSegmentMember.Declarative ->
                    il.Add(CilInstruction CilOpCodes.Nop)
            | bad -> raise(System.NotImplementedException(sprintf "Add translation implementation for %A" bad))
