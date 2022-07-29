namespace Wacil.Compiler.Wasm.Validation

open System.Collections.Immutable
open System.Collections.Generic
open System.Runtime.CompilerServices

open Wacil.Compiler.Helpers
open Wacil.Compiler.Helpers.Collections

open Wacil.Compiler.Wasm
open Wacil.Compiler.Wasm.Validation.Table

[<AutoOpen>]
module Helpers =
    let inline createImportOrDefinitionLookup
        (imports: ImmutableArray<_>)
        (definitions: ImmutableArray<_>)
        importMap
        definitionMap
        =
        let lookup = Array.zeroCreate<'Any>(imports.Length + definitions.Length)
        fun (index: 'Index) ->
            let index' = int32 index
            if Unsafe.isReferenceNull lookup[index'] then
                lookup[index'] <-
                    if index' < imports.Length then
                        importMap(index', imports[index'])
                    else
                        let index'' = index' - imports.Length
                        definitionMap(index'', definitions[index''])
            lookup[index']

[<Sealed>]
type ValidModule
    (
        custom: ImmutableArray<Format.Custom>,
        types: ImmutableArray<Format.FuncType>,
        functions: ImmutableArray<Function>,
        tables: ImmutableArray<Format.TableType>,
        imports: ModuleImportLookup,
        memories: ImmutableArray<Format.Limits>,
        globals: ImmutableArray<Global>,
        exports: ModuleExportLookup,
        start: Format.FuncIdx voption,
        elements: ImmutableArray<ValidElement>,
        data: ImmutableArray<ValidData>
    )
    =
    let anyFunctionLookup =
        createImportOrDefinitionLookup imports.Imports.Functions functions AnyFunction.Import AnyFunction.Defined

    let anyTableLookup =
        createImportOrDefinitionLookup imports.Imports.Tables tables AnyTable.Import AnyTable.Defined

    let anyMemoryLookup =
        createImportOrDefinitionLookup imports.Imports.Memories memories AnyMemory.Import AnyMemory.Defined

    let anyGlobalLookup =
        createImportOrDefinitionLookup imports.Imports.Globals globals AnyGlobal.Import AnyGlobal.Defined

    member _.CustomSections = custom
    member _.Types = types
    member _.Imports = imports
    member _.Functions = functions
    member _.Tables = tables
    member _.Memories = memories
    member _.Globals = globals
    member _.Exports = exports
    member _.Start = start
    member _.Elements = elements
    member _.Data = data

    member _.GetFunction(index: Format.FuncIdx) = anyFunctionLookup index
    member _.GetTable(index: Format.TableIdx) = anyTableLookup index
    member _.GetMemory(index: Format.MemIdx) = anyMemoryLookup index
    member _.GetGlobal(index: Format.GlobalIdx) = anyGlobalLookup index

type ValidationException =
    inherit System.Exception

    new (message: string) = { inherit System.Exception(message) }

[<Sealed>]
type DuplicateSectionException (section: Format.SectionId) =
    inherit ValidationException(sprintf "The %O section already exists" section)

    member _.Section = section

[<Sealed>]
type InvalidSectionOrderException (current: Format.SectionId, next: Format.SectionId) =
    inherit ValidationException(sprintf "The %O section must be placed before the %O section" current next)

    member _.CurrentSection = current
    member _.NextSection = next

[<Sealed>]
type FunctionSectionCountException (section: Format.SectionId, expected: int, actual: int) =
    inherit
        ValidationException(
            sprintf
                "The %O section used for functions was expected to contain %i items, but got %i items"
                section
                expected
                actual
        )

    member _.Section = section
    member _.ExpectedCount = expected
    member _.ActualCount = actual

[<Sealed>]
type DataSegmentCountException (expected: int, actual: int) =
    inherit ValidationException(sprintf "Expected %i data segments but got %i" expected actual)

    member _.Expected = expected
    member _.Actual = actual

[<Sealed>]
type DuplicateExportException (name: string) =
    inherit ValidationException(sprintf "An export corresponding to the name \"%s\" already exists" name)

    member _.Name = name

[<Sealed>]
type OperandStackUnderflowException () =
    inherit ValidationException(sprintf "The operand stack was unexpectedly empty")

[<Sealed>]
type OperandTypeMismatchException (expected: OperandType, actual: OperandType) =
    inherit ValidationException(sprintf "Expected type %O but got %O" expected actual)

    member _.Expected = expected
    member _.Actual = actual

[<Sealed>]
type ControlFrameStackUnderflowException () =
    inherit ValidationException(sprintf "The control frame stack was unexpectedly empty")

[<Sealed>]
type ElseInstructionMismatchException (index: int) =
    inherit ValidationException(sprintf "Expected matching if instruction for else instruction at index %i" index)

    member _.Index = index

[<Sealed>]
type GlobalIsNotMutableException (index: Format.GlobalIdx) =
    inherit ValidationException(sprintf "The global variable at index %i is not mutable" (int32 index))

    member _.Index = index

[<Sealed>]
type TableElementTypeMismatchException (table: Format.TableIdx, expected: Format.RefType, actual: Format.RefType) =
    inherit ValidationException(sprintf "Expected table #%i to contain elements of type %O but got %O" (int32 table) expected actual)

    member _.Table = table
    member _.Expected = expected
    member _.Actual = actual

[<Sealed>]
type ExpectedPassiveDataSegmentException (index: Format.DataIdx) =
    inherit ValidationException(sprintf "Expected data segment #%i to be a passive data segment" (int32 index))

    member _.Index = index

module Validate =
    [<IsReadOnly; Struct; RequireQualifiedAccess; StructuralComparison; StructuralEquality>]
    type SectionOrder =
        | Type
        | Import
        | Function
        | Table
        | Memory
        | Global
        | Export
        | Start
        | Element
        | DataCount
        | Code
        | Data

        member this.Id =
            match this with
            | Type -> Format.SectionId.Type
            | Import -> Format.SectionId.Import
            | Function -> Format.SectionId.Function
            | Table -> Format.SectionId.Table
            | Memory -> Format.SectionId.Memory
            | Global -> Format.SectionId.Global
            | Export -> Format.SectionId.Export
            | Start -> Format.SectionId.Start
            | Element -> Format.SectionId.Element
            | DataCount -> Format.SectionId.DataCount
            | Code -> Format.SectionId.Code
            | Data -> Format.SectionId.Data

    [<IsByRefLike; Struct; NoComparison; NoEquality>]
    type ValidModuleBuilder =
        { mutable CustomSections: ArrayBuilder<Format.Custom>
          mutable Types: ImmutableArray<Format.FuncType> voption
          mutable Imports: ImmutableArray<Format.Import> voption
          mutable Functions: ImmutableArray<Format.TypeIdx> voption
          mutable Tables: ImmutableArray<Format.TableType> voption
          mutable Memories: ImmutableArray<Format.Limits> voption
          mutable Globals: ImmutableArray<Format.Global> voption
          mutable Exports: ImmutableArray<Format.Export> voption
          mutable Start: Format.FuncIdx voption
          mutable Elements: ImmutableArray<Format.Element> voption 
          mutable Code: ImmutableArray<Format.Code> voption
          mutable DataCount: uint32 voption
          mutable Data: ImmutableArray<Format.Data> voption }

    [<IsReadOnly; Struct; RequireQualifiedAccess; NoComparison; StructuralEquality>]
    type ControlFlow =
        | Block
        | If
        | Else
        | Loop
        | Normal

    [<NoComparison; ReferenceEquality>]
    type ControlFrame =
        { /// The types at the top of the operand stack when the block is entered.
          StartTypes: ImmutableArray<Format.ValType>
          EndTypes: ImmutableArray<Format.ValType>
          StartHeight: uint32
          ControlFlow: ControlFlow
          mutable Unreachable: bool }

    let mapToOperandTypes (types: ImmutableArray<Format.ValType>) =
        let mapped = Array.zeroCreate types.Length
        for i = 0 to types.Length - 1 do mapped[i] <- ValType types[i]
        Unsafe.Array.toImmutable mapped

    let getVariableType (expression: ValidExpression) index =
        match expression.TryGetLocal index with
        | true, ty -> ValType ty
        | false, _ -> raise(System.ArgumentOutOfRangeException(nameof index, index, "Local variable was not defined"))

    /// Implementation of the WebAssembly instruction validation algorithm.
    [<Sealed>]
    type InstructionValidator (mdle: ValidModule) =
        let mutable valueTypeStack = ArrayBuilder<OperandType>.Create()
        let mutable controlFrameStack = ArrayBuilder<ControlFrame>.Create()
        let mutable validInstructonBuilder = ArrayBuilder<ValidInstruction>.Create()
        let mutable maximumIntroducedBlockCount = 0

        member _.GetCurrentValueTypes() = valueTypeStack.CopyToImmutableArray()

        member _.PushValue ty = valueTypeStack.Add ty

        member this.PushManyValues(types: ImmutableArray<_>) =
            for t in types do this.PushValue(ValType t)

        member _.PopValue() = 
            if controlFrameStack.IsEmpty then
                if valueTypeStack.IsEmpty then raise(OperandStackUnderflowException())
                let popped = valueTypeStack.Pop()
                popped
            else
                let frame = controlFrameStack.LastRef()
                if uint32 valueTypeStack.Length = frame.StartHeight then
                    if not frame.Unreachable then raise(OperandStackUnderflowException())
                    UnknownType
                else
                    let popped = valueTypeStack.Pop()
                    popped

        member this.PopValue expected =
            let actual = this.PopValue()
            if actual <> expected && actual <> UnknownType && expected <> UnknownType then
                raise(OperandTypeMismatchException(expected, actual))

        member this.PopManyValues(expected: ImmutableArray<_>) =
            for i = expected.Length - 1 downto 0 do
                this.PopValue(ValType expected.[i])

        member this.PushControlFrame(controlFlowKind, input, output) =
            controlFrameStack.Add
                { ControlFrame.StartTypes = input
                  EndTypes = output
                  StartHeight = uint32 valueTypeStack.Length
                  ControlFlow = controlFlowKind
                  Unreachable = false }

            this.PushManyValues input

            if controlFrameStack.Length > maximumIntroducedBlockCount then
                maximumIntroducedBlockCount <- controlFrameStack.Length

        member this.PopControlFrame() =
            if controlFrameStack.IsEmpty then raise(ControlFrameStackUnderflowException())
            let frame = controlFrameStack.Pop()
            this.PopManyValues frame.EndTypes |> ignore // Could avoid allocation of popped types array here
            if uint32 valueTypeStack.Length <> frame.StartHeight then raise(OperandStackUnderflowException())
            frame

        member _.LabelTypes frame =
            match frame.ControlFlow with
            | ControlFlow.Loop _ -> frame.StartTypes
            | ControlFlow.Block _ | ControlFlow.If _ | ControlFlow.Else _ -> frame.EndTypes
            | ControlFlow.Normal -> invalidOp "cannot obtain label types for instructio that does not affect control flow"

        member _.MarkUnreachable() =
            if controlFrameStack.IsEmpty then raise(ControlFrameStackUnderflowException())
            let frame = controlFrameStack.LastRef()
            valueTypeStack.ResizeWithDefault(int32 frame.StartHeight)
            frame.Unreachable <- true

        member _.CheckBranchTarget(target: Format.LabelIdx) =
            if controlFrameStack.Length < int32 target then
                failwithf
                    "TODO: Add exception type for bad branch target (length = %i, target = %i)"
                    controlFrameStack.Length
                    (int32 target)
            Checked.int32 target

        member _.GetBlockType ty =
            match ty with
            | Format.BlockType.Void -> Format.FuncType.empty
            | Format.BlockType.Val rt -> Format.FuncType.ofReturnType rt
            | Format.BlockType.Index i -> mdle.Types[Checked.int32 i]

        member this.CheckUnconditionalBranch target =
            this.PopManyValues(this.LabelTypes(controlFrameStack.ItemFromEnd target))
            this.MarkUnreachable()

        member _.GetTableType(Format.TableIdx index) =
            // TODO: Check table imports as well
            mdle.Tables[index].ElementType

        member this.CheckTableType(index, expected) =
            let tableElementType = this.GetTableType index
            if tableElementType <> expected then
                raise(TableElementTypeMismatchException(index, expected, tableElementType))

        member this.Validate(expression: ValidExpression) =
            // Reset the validator
            valueTypeStack.Clear()
            controlFrameStack.ClearWithDefault()
            validInstructonBuilder.Clear()
            maximumIntroducedBlockCount <- 0

            // All expressions implictly define a block
            controlFrameStack.Add
                { ControlFrame.ControlFlow = ControlFlow.Block
                  StartTypes = ImmutableArray.Empty
                  EndTypes = expression.ResultTypes
                  StartHeight = 0u
                  Unreachable = false }

            for index = 0 to expression.Source.Length - 1 do
                let instruction = expression.Source[index]

                match instruction with
                | Format.Unreachable -> this.MarkUnreachable() // TODO: Will ValidInstruction.PushedTypes be valid here?
                | Format.Nop | Format.DataDrop _ | Format.ElemDrop _ -> ()
                | Format.Block ty ->
                    let ty' = this.GetBlockType ty
                    this.PopManyValues ty'.Parameters
                    this.PushControlFrame(ControlFlow.Block, ty'.Parameters, ty'.Results)
                | Format.Loop ty ->
                    let ty' = this.GetBlockType ty
                    this.PopManyValues ty'.Parameters
                    this.PushControlFrame(ControlFlow.Loop, ty'.Parameters, ty'.Results)
                | Format.If ty ->
                    let ty' = this.GetBlockType ty
                    this.PopValue OperandType.i32
                    this.PopManyValues ty'.Parameters
                    this.PushControlFrame(ControlFlow.If, ty'.Parameters, ty'.Results)
                | Format.Else ->
                    let frame = this.PopControlFrame()
                    match frame.ControlFlow with
                    | ControlFlow.If -> this.PushControlFrame(ControlFlow.Else, frame.StartTypes, frame.EndTypes)
                    | _ -> raise(ElseInstructionMismatchException index)
                | Format.End -> this.PushManyValues(this.PopControlFrame().EndTypes)
                | Format.Br target -> this.CheckUnconditionalBranch(this.CheckBranchTarget target)
                | Format.BrIf target ->
                    let target' = this.CheckBranchTarget target
                    this.PopValue OperandType.i32
                    let labelTypes = this.LabelTypes(controlFrameStack.ItemFromEnd target')
                    this.PopManyValues labelTypes
                    this.PushManyValues labelTypes
                | Format.BrTable(targets, defaultTarget) ->
                    this.PopValue OperandType.i32
                    let defaultTarget' = this.CheckBranchTarget defaultTarget
                    let defaultTypes = this.LabelTypes(controlFrameStack.ItemFromEnd defaultTarget')
                    let arity = defaultTypes.Length
                    for label in targets do
                        let label' = this.CheckBranchTarget label
                        let labelTypes = this.LabelTypes(controlFrameStack.ItemFromEnd label')
                        if labelTypes.Length <> arity then failwith "TODO: Error for bad switch"
                        this.PopManyValues labelTypes
                        this.PushManyValues labelTypes
                    this.PopManyValues defaultTypes
                    this.MarkUnreachable()
                | Format.Return -> this.CheckUnconditionalBranch(controlFrameStack.Length - 1) // Branch to the implicit outermost block
                | Format.Call callee ->
                    let ty = mdle.GetFunction(callee).Type
                    this.PopManyValues ty.Parameters
                    this.PushManyValues ty.Results
                | Format.CallIndirect(ty, table) ->
                    this.PopValue OperandType.i32
                    let ty' = mdle.Types[Checked.int32 ty]
                    this.PopManyValues ty'.Parameters
                    this.PushManyValues ty'.Results
                    this.CheckTableType(table, Format.FuncRef)
                | Format.Drop -> this.PopValue() |> ignore
                | Format.Select ->
                    this.PopValue OperandType.i32
                    match this.PopValue(), this.PopValue() with
                    | t1, t2 when not(OperandType.isNumType t1 && OperandType.isNumType t2) || (OperandType.isVecType t1 && OperandType.isVecType t2) ->
                        failwithf "bad select type %A and %A" t1 t2
                    | t1, t2 when t1 <> t2 && t1 <> UnknownType && t2 <> UnknownType ->
                        failwithf "select type mismatch (expected %A, got %A)" t1 t2
                    | UnknownType, t2 -> this.PushValue t2
                    | t1, _ -> this.PushValue t1
                | Format.LocalGet i -> this.PushValue(getVariableType expression i)
                | Format.LocalSet i -> this.PopValue(getVariableType expression i)
                | Format.LocalTee i ->
                    let ty = getVariableType expression i
                    this.PopValue ty
                    this.PushValue ty
                | Format.GlobalGet i -> this.PushValue(ValType mdle.Globals[Checked.int32 i].Type.Type)
                | Format.GlobalSet i ->
                    let glbl = mdle.Globals[Checked.int32 i]
                    if glbl.Type.Mutability <> Format.Mutability.Var then raise(GlobalIsNotMutableException i)
                    this.PopValue(ValType glbl.Type.Type)
                | Format.TableGet table ->
                    this.PopValue OperandType.i32
                    this.PushValue(OperandType.fromRefType(this.GetTableType table))
                | Format.TableSet table ->
                    this.PopValue(OperandType.fromRefType(this.GetTableType table))
                    this.PopValue OperandType.i32
                | Format.I32Load _ | Format.I32Load8S _ | Format.I32Load8U _ | Format.I32Load16S _ | Format.I32Load16U _
                | Format.I32Eqz | Format.I32Clz | Format.I32Ctz | Format.I32Popcnt | Format.I32Extend8S | Format.I32Extend16S ->
                    this.PopValue OperandType.i32
                    this.PushValue OperandType.i32
                | Format.MemoryGrow memory ->
                    mdle.GetMemory memory |> ignore
                    this.PopValue OperandType.i32
                    this.PushValue OperandType.i32
                | Format.I64Load _ | Format.I64Load8S _ | Format.I64Load8U _ | Format.I64Load16S _ | Format.I64Load16U _
                | Format.I64Load32S _ | Format.I64Load32U _ | Format.I64ExtendI32S | Format.I64ExtendI32U ->
                    this.PopValue OperandType.i32
                    this.PushValue OperandType.i64
                | Format.F32Load _ | Format.F32ConvertI32S | Format.F32ConvertI32U | Format.F32ReinterpretI32 ->
                    this.PopValue OperandType.i32
                    this.PushValue OperandType.f32
                | Format.F64Load _ | Format.F64ConvertI32S | Format.F64ConvertI32U ->
                    this.PopValue OperandType.i32
                    this.PushValue OperandType.f64
                | Format.I32Store _ | Format.I32Store8 _ | Format.I32Store16 _ ->
                    this.PopValue OperandType.i32
                    this.PopValue OperandType.i32
                | Format.I64Store _ | Format.I64Store8 _ | Format.I64Store16 _ | Format.I64Store32 _ ->
                    this.PopValue OperandType.i64
                    this.PopValue OperandType.i32
                | Format.F32Store _ ->
                    this.PopValue OperandType.f32
                    this.PopValue OperandType.i32
                | Format.F64Store _ ->
                    this.PopValue OperandType.f64
                    this.PopValue OperandType.i32
                | Format.I32Const _ | Format.TableSize _ -> this.PushValue OperandType.i32
                | Format.MemorySize memory ->
                    mdle.GetMemory memory |> ignore
                    this.PushValue OperandType.i32
                | Format.I64Const _ -> this.PushValue OperandType.i64
                | Format.F32Const _ -> this.PushValue OperandType.f32
                | Format.F64Const _ -> this.PushValue OperandType.f64
                | Format.I32Eq | Format.I32Ne | Format.I32LtS | Format.I32LtU | Format.I32GtS | Format.I32GtU | Format.I32LeS
                | Format.I32LeU | Format.I32GeS | Format.I32GeU | Format.I32Add | Format.I32Sub | Format.I32Mul | Format.I32DivS
                | Format.I32DivU | Format.I32RemS | Format.I32RemU | Format.I32And | Format.I32Or | Format.I32Xor | Format.I32Shl
                | Format.I32ShrS | Format.I32ShrU | Format.I32Rotl | Format.I32Rotr ->
                    this.PopValue OperandType.i32
                    this.PopValue OperandType.i32
                    this.PushValue OperandType.i32
                | Format.I64Eqz | Format.I32WrapI64 ->
                    this.PopValue OperandType.i64
                    this.PushValue OperandType.i32
                | Format.I64Eq | Format.I64Ne | Format.I64LtS | Format.I64LtU | Format.I64GtS | Format.I64GtU | Format.I64LeS
                | Format.I64LeU | Format.I64GeS | Format.I64GeU ->
                    this.PopValue OperandType.i64
                    this.PopValue OperandType.i64
                    this.PushValue OperandType.i32
                | Format.F32Eq | Format.F32Ne | Format.F32Lt | Format.F32Gt | Format.F32Le | Format.F32Ge ->
                    this.PopValue OperandType.f32
                    this.PopValue OperandType.f32
                    this.PushValue OperandType.i32
                | Format.F64Eq | Format.F64Ne | Format.F64Lt | Format.F64Gt | Format.F64Le | Format.F64Ge ->
                    this.PopValue OperandType.f64
                    this.PopValue OperandType.f64
                    this.PushValue OperandType.i32
                | Format.I64Clz | Format.I64Ctz | Format.I64Popcnt | Format.I64Extend8S | Format.I64Extend16S
                | Format.I64Extend32S ->
                    this.PopValue OperandType.i64
                    this.PushValue OperandType.i64
                | Format.I64Add | Format.I64Sub | Format.I64Mul | Format.I64DivS | Format.I64DivU | Format.I64RemS
                | Format.I64RemU | Format.I64And | Format.I64Or | Format.I64Xor | Format.I64Shl | Format.I64ShrS | Format.I64ShrU
                | Format.I64Rotl | Format.I64Rotr ->
                    this.PopValue OperandType.i64
                    this.PopValue OperandType.i64
                    this.PushValue OperandType.i64
                | Format.F32Abs | Format.F32Neg | Format.F32Ceil | Format.F32Floor | Format.F32Trunc | Format.F32Nearest
                | Format.F32Sqrt ->
                    this.PopValue OperandType.f32
                    this.PushValue OperandType.f32
                | Format.F32Add | Format.F32Sub | Format.F32Mul | Format.F32Mul | Format.F32Div | Format.F32Min | Format.F32Max
                | Format.F32Copysign ->
                    this.PopValue OperandType.f32
                    this.PopValue OperandType.f32
                    this.PushValue OperandType.f32
                | Format.F64Abs | Format.F64Neg | Format.F64Ceil | Format.F64Floor | Format.F64Trunc | Format.F64Nearest
                | Format.F64Sqrt ->
                    this.PopValue OperandType.f64
                    this.PushValue OperandType.f64
                | Format.F64Add | Format.F64Sub | Format.F64Mul | Format.F64Mul | Format.F64Div | Format.F64Min | Format.F64Max
                | Format.F64Copysign ->
                    this.PopValue OperandType.f64
                    this.PopValue OperandType.f64
                    this.PushValue OperandType.f64
                | Format.I32TruncF32S | Format.I32TruncF32U | Format.I32ReinterpretF32 | Format.I32TruncSatF32S
                | Format.I32TruncSatF32U ->
                    this.PopValue OperandType.f32
                    this.PushValue OperandType.i32
                | Format.I32TruncF64S | Format.I32TruncF64U | Format.I32TruncSatF64S | Format.I32TruncSatF64U ->
                    this.PopValue OperandType.f64
                    this.PushValue OperandType.i32
                | Format.I64TruncF32S | Format.I64TruncF32U | Format.I64TruncSatF32S | Format.I64TruncSatF32U ->
                    this.PopValue OperandType.f32
                    this.PushValue OperandType.i64
                | Format.I64TruncF64S | Format.I64TruncF64U | Format.I64ReinterpretF64 | Format.I64TruncSatF64S
                | Format.I64TruncSatF64U ->
                    this.PopValue OperandType.f64
                    this.PushValue OperandType.i64
                | Format.F32ConvertI64S | Format.F32ConvertI64U ->
                    this.PopValue OperandType.i64
                    this.PushValue OperandType.f32
                | Format.F32DemoteF64 ->
                    this.PopValue OperandType.f64
                    this.PushValue OperandType.f32
                | Format.F64ConvertI64S | Format.F64ConvertI64U | Format.F64ReinterpretI64 ->
                    this.PopValue OperandType.i64
                    this.PushValue OperandType.f64
                | Format.F64PromoteF32 ->
                    this.PopValue OperandType.f32
                    this.PushValue OperandType.f64
                | Format.RefNull rtype -> this.PushValue(OperandType.fromRefType rtype)
                | Format.RefIsNull ->
                    let popped = this.PopValue()
                    if not(OperandType.isRefType popped) then failwithf "%A is not a ref type" popped
                    this.PushValue OperandType.i32
                | Format.RefFunc _ -> this.PushValue OperandType.funcref
                | Format.MemoryInit(data, memory) ->
                    match mdle.Data[int32 data].Mode with
                    | ValueSome _ -> raise(ExpectedPassiveDataSegmentException data) 
                    | ValueNone -> ()

                    mdle.GetMemory memory |> ignore
                    this.PopValue OperandType.i32
                    this.PopValue OperandType.i32
                    this.PopValue OperandType.i32
                | Format.MemoryCopy(x, y) ->
                    mdle.GetMemory x |> ignore
                    mdle.GetMemory y |> ignore
                    this.PopValue OperandType.i32
                    this.PopValue OperandType.i32
                    this.PopValue OperandType.i32
                | Format.MemoryFill memory ->
                    mdle.GetMemory memory |> ignore
                    this.PopValue OperandType.i32
                    this.PopValue OperandType.i32
                    this.PopValue OperandType.i32
                | Format.TableCopy _ ->
                    this.PopValue OperandType.i32
                    this.PopValue OperandType.i32
                    this.PopValue OperandType.i32
                | Format.TableInit(element, _) ->
                    //mdle.Elements
                    // TODO: Check that element type is correct

                    this.PopValue OperandType.i32
                    this.PopValue OperandType.i32
                    this.PopValue OperandType.i32
                | Format.TableGrow table ->
                    this.PopValue OperandType.i32
                    this.PopValue(OperandType.fromRefType(this.GetTableType table))
                    this.PushValue OperandType.i32
                | Format.TableFill table ->
                    this.PopValue OperandType.i32
                    this.PopValue(OperandType.fromRefType(this.GetTableType table))
                    this.PopValue OperandType.i32

                validInstructonBuilder.Add
                    { ValidInstruction.Instruction = instruction
                      Unreachable =
                        if not controlFrameStack.IsEmpty
                        then controlFrameStack.LastRef().Unreachable
                        else false }

            expression.SetInstructions(validInstructonBuilder.CopyToImmutableArray())
            expression.SetMaximumIntroducedBlockCount maximumIntroducedBlockCount

    let fromModuleSections (sections: ImmutableArray<Format.Section>) =
        let mutable contents =
            { CustomSections = ArrayBuilder<_>.Create()
              Types = ValueNone
              Imports = ValueNone
              Functions = ValueNone
              Tables = ValueNone
              Memories = ValueNone
              Globals = ValueNone
              Exports = ValueNone
              Start = ValueNone
              Elements = ValueNone
              DataCount = ValueNone
              Code = ValueNone
              Data = ValueNone }

        do // Check that sections are in the correct order
            let mutable order = SectionOrder.Type
            for s in sections do
                match s with
                | Format.Section.Custom custom -> contents.CustomSections.Add custom
                | Format.Section.Type types ->
                    if order > SectionOrder.Type then raise(InvalidSectionOrderException(Format.SectionId.Type, order.Id))
                    else if contents.Types.IsSome then raise(DuplicateSectionException Format.SectionId.Type)
                    else
                        order <- SectionOrder.Import
                        contents.Types <- ValueSome types
                | Format.Section.Import imports ->
                    if order > SectionOrder.Import then raise(InvalidSectionOrderException(Format.SectionId.Import, order.Id))
                    else if contents.Imports.IsSome then raise(DuplicateSectionException Format.SectionId.Import)
                    else
                        order <- SectionOrder.Function
                        contents.Imports <- ValueSome imports
                | Format.Section.Function functions ->
                    if order > SectionOrder.Function then
                        raise(InvalidSectionOrderException(Format.SectionId.Function, order.Id))
                    else if contents.Functions.IsSome then
                        raise(DuplicateSectionException Format.SectionId.Function)
                    else
                        order <- SectionOrder.Table
                        contents.Functions <- ValueSome functions
                | Format.Section.Table tables ->
                    if order > SectionOrder.Table then raise(InvalidSectionOrderException(Format.SectionId.Table, order.Id))
                    else if contents.Tables.IsSome then raise(DuplicateSectionException Format.SectionId.Table)
                    else
                        order <- SectionOrder.Memory
                        contents.Tables <- ValueSome tables
                | Format.Section.Memory memory ->
                    if order > SectionOrder.Memory then raise(InvalidSectionOrderException(Format.SectionId.Memory, order.Id))
                    else if contents.Memories.IsSome then raise(DuplicateSectionException Format.SectionId.Memory)
                    else
                        order <- SectionOrder.Global
                        contents.Memories <- ValueSome memory
                | Format.Section.Global globals ->
                    if order > SectionOrder.Global then raise(InvalidSectionOrderException(Format.SectionId.Global, order.Id))
                    else if contents.Globals.IsSome then raise(DuplicateSectionException Format.SectionId.Global)
                    else
                        order <- SectionOrder.Export
                        contents.Globals <- ValueSome globals
                | Format.Section.Export exports ->
                    if order > SectionOrder.Export then raise(InvalidSectionOrderException(Format.SectionId.Export, order.Id))
                    else if contents.Exports.IsSome then raise(DuplicateSectionException Format.SectionId.Export)
                    else
                        order <- SectionOrder.Start
                        contents.Exports <- ValueSome exports
                | Format.Section.Start start ->
                    if order > SectionOrder.Start then raise(InvalidSectionOrderException(Format.SectionId.Start, order.Id))
                    else if contents.Start.IsSome then raise(DuplicateSectionException Format.SectionId.Start)
                    else
                        order <- SectionOrder.Element
                        contents.Start <- ValueSome start
                | Format.Section.Element elements ->
                    if order > SectionOrder.Element then raise(InvalidSectionOrderException(Format.SectionId.Element, order.Id))
                    else if contents.Elements.IsSome then raise(DuplicateSectionException Format.SectionId.Element)
                    else
                        order <- SectionOrder.DataCount
                        contents.Elements <- ValueSome elements
                | Format.Section.DataCount count ->
                    if order > SectionOrder.DataCount then
                        raise(InvalidSectionOrderException(Format.SectionId.DataCount, order.Id))
                    else if contents.DataCount.IsSome then
                        raise(DuplicateSectionException Format.SectionId.DataCount)
                    else
                        order <- SectionOrder.Code
                        contents.DataCount <- ValueSome count
                | Format.Section.Code code ->
                    if order > SectionOrder.Code then raise(InvalidSectionOrderException(Format.SectionId.Code, order.Id))
                    else if contents.Code.IsSome then raise(DuplicateSectionException Format.SectionId.Code)
                    else
                        order <- SectionOrder.Data
                        contents.Code <- ValueSome code
                | Format.Section.Data data ->
                    if contents.Data.IsSome then raise(DuplicateSectionException Format.SectionId.Data)
                    else
                        order <- SectionOrder.Export
                        contents.Data <- ValueSome data

        let inline itemsOrEmpty items = ValueOption.defaultValue ImmutableArray.Empty items

        // The order used to validate a module's sections as in the specification is not exactly followed here

        let types = itemsOrEmpty contents.Types
        let tables = itemsOrEmpty contents.Tables
        let memories = itemsOrEmpty contents.Memories

        let imports =
            // Group all imports by module name
            let moduleImportLookup, actualModuleImports =
                let imports = itemsOrEmpty contents.Imports
                let lookup = if imports.IsEmpty then null else Dictionary(capacity = imports.Length)

                let mutable functionImportIndex = 0
                let mutable tableImportIndex = 0
                let mutable memoryImportIndex = 0
                let mutable globalImportIndex = 0

                // Loop is skipped if lookup is null
                for import in imports do
                    let entries =
                        match lookup.TryGetValue import.Module with
                        | true, entries' -> entries'
                        | false, _ ->
                            let entries' = List()
                            lookup[import.Module] <- entries'
                            entries'

                    let index =
                        match import.Description with
                        | Format.ImportDesc.Func _ ->
                            let i = functionImportIndex
                            functionImportIndex <- functionImportIndex + 1
                            i
                        | Format.ImportDesc.Table _ ->
                            let i = tableImportIndex
                            tableImportIndex <- tableImportIndex + 1
                            i
                        | Format.ImportDesc.Mem _ ->
                            let i = memoryImportIndex
                            memoryImportIndex <- memoryImportIndex + 1
                            i
                        | Format.ImportDesc.Global _ ->
                            let i = globalImportIndex
                            globalImportIndex <- globalImportIndex + 1
                            i

                    entries.Add(struct(index, import))
                lookup, if isNull lookup then null else SortedDictionary()

            let mutable allFunctionImports = ArrayBuilder<FunctionImport>.Create()
            let mutable allTableImports = ArrayBuilder<TableImport>.Create()
            let mutable allMemoryImports = ArrayBuilder<MemoryImport>.Create()
            let mutable allGlobalImports = ArrayBuilder<GlobalImport>.Create()

            if not(isNull moduleImportLookup) then
                let mutable matchingFunctionImports = ArrayBuilder<FunctionImport>.Create()
                let mutable matchingTableImports = ArrayBuilder<TableImport>.Create()
                let mutable matchingMemoryImports = ArrayBuilder<MemoryImport>.Create()
                let mutable matchingGlobalImports = ArrayBuilder<GlobalImport>.Create()

                for KeyValue(importModuleName, moduleImports) in moduleImportLookup do
                    matchingFunctionImports.Clear()
                    matchingTableImports.Clear()
                    matchingMemoryImports.Clear()
                    matchingGlobalImports.Clear()

                    for (i, import) in moduleImports do
                        match import.Description with
                        | Format.ImportDesc.Func index ->
                            let func =
                                { FunctionImport.Index = Format.FuncIdx i
                                  FunctionImport.Name = import.Name
                                  FunctionImport.Type = types[Checked.int32 index] }

                            matchingFunctionImports.Add func
                            allFunctionImports.Add func
                        | Format.ImportDesc.Table ty ->
                            let table =
                                { TableImport.Index = Format.TableIdx i
                                  TableImport.Name = import.Name
                                  TableImport.Type = ty }

                            matchingTableImports.Add table
                            allTableImports.Add table
                        | Format.ImportDesc.Mem limits ->
                            let memory =
                                { MemoryImport.Index = Format.MemIdx i
                                  MemoryImport.Name = import.Name
                                  MemoryImport.Limits = limits }

                            matchingMemoryImports.Add memory
                            allMemoryImports.Add memory
                        | Format.ImportDesc.Global ty ->
                            let glbl =
                                { GlobalImport.Index = Format.GlobalIdx i
                                  GlobalImport.Name = import.Name
                                  GlobalImport.Type = ty }

                            matchingGlobalImports.Add glbl
                            allGlobalImports.Add glbl

                    actualModuleImports[importModuleName] <- 
                        { ModuleImports.Functions = matchingFunctionImports.ToImmutableArray()
                          ModuleImports.Tables = matchingTableImports.ToImmutableArray()
                          ModuleImports.Memories = matchingMemoryImports.ToImmutableArray()
                          ModuleImports.Globals = matchingGlobalImports.ToImmutableArray() }

            let allModuleImports =
                { ModuleImports.Functions = allFunctionImports.ToImmutableArray()
                  ModuleImports.Tables = allTableImports.ToImmutableArray()
                  ModuleImports.Memories = allMemoryImports.ToImmutableArray()
                  ModuleImports.Globals = allGlobalImports.ToImmutableArray() }

            ModuleImportLookup(actualModuleImports, allModuleImports)

        let globals =
            let moduleGlobalDefinitions = itemsOrEmpty contents.Globals
            let mutable globals = Array.zeroCreate moduleGlobalDefinitions.Length
            for i = 0 to globals.Length - 1 do
                let glbl = moduleGlobalDefinitions[i]
                globals[i] <-
                    { Global.Type = glbl.Type
                      Global.Value = ValidExpression(
                        glbl.Expression,
                        ImmutableArray.Empty,
                        ImmutableArray.Empty,
                        ImmutableArray.Create glbl.Type.Type
                      ) }
            Unsafe.Array.toImmutable globals

        let functions =
            let moduleFunctionTypes = itemsOrEmpty contents.Functions
            let moduleFunctionBodies = itemsOrEmpty contents.Code
            let expectedFunctionCount = moduleFunctionTypes.Length
            let mutable localTypesBuilder = ArrayBuilder<Format.ValType>.Create()

            if expectedFunctionCount <> moduleFunctionBodies.Length then
                raise(FunctionSectionCountException(Format.SectionId.Code, expectedFunctionCount, moduleFunctionBodies.Length))
                
            let mutable moduleFunctionDefinitions = ArrayBuilder<Function>.Create expectedFunctionCount

            for i = 0 to expectedFunctionCount - 1 do
                let body = moduleFunctionBodies[i]
                
                localTypesBuilder.Clear()
                for local in body.Locals do
                    for i = 0 to int local.Count - 1 do
                        localTypesBuilder.Add local.Type
                
                let functionType = types[moduleFunctionTypes[i] |> Checked.int32]

                moduleFunctionDefinitions.Add
                    { Function.Type = functionType
                      Body = ValidExpression(
                        body.Body,
                        functionType.Parameters,
                        localTypesBuilder.ToImmutableArray(),
                        functionType.Results
                      ) }

            moduleFunctionDefinitions.ToImmutableArray()

        let data =
            let data = itemsOrEmpty contents.Data
            let expectedDataCount = ValueOption.defaultValue data.Length (ValueOption.map Checked.int32 contents.DataCount)
            // Some tools such as wat2wasm omit the data count even when data segments are present
            // A better way to validate the counts would be to use the expectedDataCount when validating the code before this point
            if expectedDataCount <> data.Length then
                raise(DataSegmentCountException(expectedDataCount, data.Length))

            let mutable segments = Array.zeroCreate data.Length
            for i = 0 to data.Length - 1 do
                let dataSegment = data[i]
                segments[i] <-
                    { ValidData.Bytes = dataSegment.Bytes
                      ValidData.Mode =
                        match dataSegment.Mode with
                        | Format.DataMode.Passive -> ValueNone
                        | Format.DataMode.Active(memory, offset) ->
                            ValueSome
                                { ValidActiveData.Memory = memory
                                  ValidActiveData.Offset = ValidExpression(
                                    offset,
                                    ImmutableArray.Empty,
                                    ImmutableArray.Empty,
                                    Format.ValType.singleI32
                                  ) } }
            Unsafe.Array.toImmutable segments

        let elements =
            let elements = itemsOrEmpty contents.Elements
            let mutable segments = Array.zeroCreate elements.Length
            let mutable elementInitializationExpressions = ArrayBuilder.Create()
            for i = 0 to elements.Length - 1 do
                let elementSegment = elements[i]

                elementInitializationExpressions.Clear()
                for init in elementSegment.Expressions do
                    elementInitializationExpressions.Add(ValidExpression(
                        init,
                        ImmutableArray.Empty,
                        ImmutableArray.Empty,
                        Format.ValType.singleOfRefType elementSegment.Type
                    ))

                segments[i] <-
                    { ValidElement.Type = elementSegment.Type
                      ValidElement.Elements = elementInitializationExpressions.ToImmutableArray()
                      ValidElement.Mode =
                        match elementSegment.Mode with
                        | Format.ElementMode.Declarative -> ValidElementMode.Declarative
                        | Format.ElementMode.Passive -> ValidElementMode.Passive
                        | Format.ElementMode.Active(table, offset) ->
                            if int32 table >= imports.Imports.Tables.Length + tables.Length then
                                failwithf "TODO: Error for table %i does not exist" (int32 table)
                            
                            let segmentOffsetExpression =
                                ValidExpression(
                                    offset,
                                    ImmutableArray.Empty,
                                    ImmutableArray.Empty,
                                    Format.ValType.singleI32
                                )

                            ValidElementMode.Active(table, segmentOffsetExpression) }
            Unsafe.Array.toImmutable segments

        let originalModuleExports = ValueOption.defaultValue ImmutableArray.Empty contents.Exports
        let moduleExportLookup = Dictionary(originalModuleExports.Length, System.StringComparer.Ordinal)
        let functionExportNames = Dictionary() // TODO: Use arrays in export lookup
        let tableExportNames = Dictionary()
        let memoryExportNames = Dictionary()
        let globalExportNames = Dictionary()

        let validated = ValidModule(
            custom = contents.CustomSections.ToImmutableArray(),
            types = types,
            imports = imports,
            functions = functions,
            tables = tables,
            memories = memories,
            globals = globals,
            exports = ModuleExportLookup(functionExportNames, tableExportNames, memoryExportNames, globalExportNames, moduleExportLookup),
            start = contents.Start,
            elements = elements,
            data = data
        )

        for e in originalModuleExports do
            match moduleExportLookup.TryGetValue e.Name with
            | true, _ -> raise(DuplicateExportException e.Name)
            | false, _ ->
                moduleExportLookup[e.Name] <-
                    match e.Description with // TODO: Figure out if WASM allows re-exporting things.
                    | Format.ExportDesc.Func index ->
                        functionExportNames.Add(index, e.Name)
                        ModuleExport.Function(index, validated.GetFunction index)
                    | Format.ExportDesc.Table index ->
                        tableExportNames.Add(index, e.Name)
                        ModuleExport.Table(index, validated.GetTable index)
                    | Format.ExportDesc.Mem index ->
                        memoryExportNames.Add(index, e.Name)
                        ModuleExport.Memory(index, validated.GetMemory index)
                    | Format.ExportDesc.Global index ->
                        globalExportNames.Add(index, e.Name)
                        ModuleExport.Global(index, validated.GetGlobal index)

        let instructionSequenceValidator = InstructionValidator validated

        for func in functions do instructionSequenceValidator.Validate func.Body

        // TODO: Check that expressions are "constant" in global values and data segments
        for glbl in globals do instructionSequenceValidator.Validate glbl.Value

        for segment in data do
            match segment.Mode with
            | ValueNone -> ()
            | ValueSome activeDataSegment -> instructionSequenceValidator.Validate activeDataSegment.Offset

        for segment in elements do
            for expr in segment.Elements do instructionSequenceValidator.Validate expr

            match segment.Mode with
            | ValidElementMode.Passive | ValidElementMode.Declarative -> ()
            | ValidElementMode.Active(_, offset) -> instructionSequenceValidator.Validate offset

        validated
