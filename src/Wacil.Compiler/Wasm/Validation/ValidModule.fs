namespace Wacil.Compiler.Wasm.Validation

open System.Collections.Immutable
open System.Collections.Generic

open Wacil.Compiler.Helpers
open Wacil.Compiler.Helpers.Collections

open Wacil.Compiler.Wasm
open Wacil.Compiler.Wasm.Validation.Table

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
        start: Table.Index voption,
        data: ImmutableArray<ValidData>
    )
    =
    member _.CustomSections = custom
    member _.Types = types
    member _.Imports = imports
    member _.Functions = functions
    member _.Tables = tables
    member _.Memories = memories
    member _.Globals = globals
    member _.Exports = exports
    member _.Start = start
    member _.Data = data

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
type ElseInstructionMismatchException (index: int, previous: Format.Instruction) =
    inherit ValidationException(
        sprintf "Expected matching if instruction for this else instruction at index %i, but got %A" index previous
    )

    member _.Index = index
    member _.PreviousStructuredInstruction = previous

[<Sealed>]
type GlobalIsNotMutableException (index: Format.Index) =
    inherit ValidationException(sprintf "The global variable at index %i is not mutable" index)

    member _.Index = index

module Validate =
    let errwith format = Printf.kprintf (fun msg -> raise(ValidationException msg)) format

    let multiMemoryNotSupported() =
        let e = System.NotSupportedException "Multiple memories are not yet supported by the Wacil compiler"
        e.HelpLink <- "https://github.com/WebAssembly/multi-memory"
        raise e

    [<Struct; RequireQualifiedAccess; StructuralComparison; StructuralEquality>]
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

    [<System.Runtime.CompilerServices.IsByRefLike; Struct; NoComparison; NoEquality>]
    type ValidModuleBuilder =
        { mutable CustomSections: ArrayBuilder<Format.Custom>
          mutable Types: ImmutableArray<Format.FuncType> voption
          mutable Imports: ImmutableArray<Format.Import> voption
          mutable Functions: ImmutableArray<Format.Index> voption
          mutable Tables: ImmutableArray<Format.TableType> voption
          mutable Memories: ImmutableArray<Format.Limits> voption
          mutable Globals: ImmutableArray<Format.Global> voption
          mutable Exports: ImmutableArray<Format.Export> voption
          mutable Start: Format.Index voption
          mutable Element: ImmutableArray<Format.Element> voption 
          mutable Code: ImmutableArray<Format.Code> voption
          mutable DataCount: uint32 voption
          mutable Data: ImmutableArray<Format.Data> voption }

    [<NoComparison; ReferenceEquality>]
    type ControlFrame =
        { Instruction: Format.Instruction
          /// The types at the top of the operand stack when the block is entered.
          StartTypes: ImmutableArray<Format.ValType>
          EndTypes: ImmutableArray<Format.ValType>
          StartHeight: uint32
          mutable Unreachable: bool }

    let mapToOperandTypes (types: ImmutableArray<Format.ValType>) =
        let mapped = Array.zeroCreate types.Length
        for i = 0 to types.Length - 1 do mapped[i] <- ValType types[i]
        Unsafe.Array.toImmutable mapped

    let getVariableType (expression: ValidExpression) (index: Format.Index) =
        match expression.TryGetLocal(Checked.int32 index) with
        | true, ty -> ValType ty
        | false, _ -> raise(System.ArgumentOutOfRangeException(nameof index, index, "Local variable was not defined"))

    /// Implementation of the WebAssembly instruction validation algorithm.
    [<Sealed>]
    type InstructionValidator
        (
            types: ImmutableArray<Format.FuncType>,
            memories: ImmutableArray<_>,
            globals: ImmutableArray<Global>
        )
        =
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
            actual

        member this.PopManyValues(expected: ImmutableArray<_>) =
            let mutable popped = Array.zeroCreate expected.Length
            for i = expected.Length - 1 downto 0 do
                popped[i] <- this.PopValue(ValType expected.[i])
            Unsafe.Array.toImmutable popped

        member this.PushControlFrame(instruction, input, output) =
            controlFrameStack.Add
                { ControlFrame.Instruction = instruction
                  StartTypes = input
                  EndTypes = output
                  StartHeight = uint32 valueTypeStack.Length
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
            match frame.Instruction with
            | Format.Loop _ -> frame.StartTypes
            | Format.Block _ | Format.If _ | Format.Else _ -> frame.EndTypes
            | bad -> failwithf "TODO: Error for not %A is not a valid structured instruction" bad

        member _.MarkUnreachable() =
            if controlFrameStack.IsEmpty then raise(ControlFrameStackUnderflowException())
            let frame = controlFrameStack.LastRef()
            valueTypeStack.ResizeWithDefault(int32 frame.StartHeight)
            frame.Unreachable <- true

        member _.CheckBranchTarget(target: Format.Index) =
            if uint32 controlFrameStack.Length < target then
                failwithf
                    "TODO: Add exception type for bad branch target (length = %i, target = %i)"
                    controlFrameStack.Length
                    target
            Checked.int32 target

        member _.GetBlockType ty =
            match ty with
            | Format.BlockType.Void -> Format.FuncType.empty
            | Format.BlockType.Val rt -> Format.FuncType.ofReturnType rt
            | Format.BlockType.Index i -> types[Checked.int32 i]

        member this.Validate(expression: ValidExpression) =
            // Reset the validator
            valueTypeStack.Clear()
            controlFrameStack.ClearWithDefault()
            validInstructonBuilder.Clear()
            maximumIntroducedBlockCount <- 0

            // All expressions implictly define a block
            controlFrameStack.Add
                { ControlFrame.Instruction =
                    match expression.ResultTypes.Length with
                    | 0 -> Format.BlockType.Void
                    | 1 -> Format.BlockType.Val expression.ResultTypes[0]
                    | _ ->
                        raise(System.NotSupportedException "TODO: Multiple return values for expressions are not yet supported")
                    |> Format.Block
                  StartTypes = ImmutableArray.Empty
                  EndTypes = expression.ResultTypes
                  StartHeight = 0u
                  Unreachable = false }

            for index = 0 to expression.Source.Length - 1 do
                let instruction = expression.Source[index]
                let mutable poppedTypes = ImmutableArray.Empty
                let mutable pushedTypes = ImmutableArray.Empty

                match instruction with
                | Format.Unreachable -> this.MarkUnreachable() // TODO: Will ValidInstruction.PushedTypes be valid here?
                | Format.Nop -> ()
                | Format.Br target ->
                    let target' = this.CheckBranchTarget target
                    poppedTypes <- mapToOperandTypes(this.LabelTypes(controlFrameStack.ItemFromEnd target'))
                    this.MarkUnreachable()
                //| Format.BrIf
                | Format.Block ty | Format.Loop ty ->
                    let ty' = this.GetBlockType ty
                    poppedTypes <- this.PopManyValues ty'.Parameters
                    this.PushControlFrame(instruction, ty'.Parameters, ty'.Results)
                | Format.If ty ->
                    let ty' = this.GetBlockType ty
                    this.PopValue OperandType.i32 |> ignore
                    poppedTypes <- OperandType.singleI32.AddRange(this.PopManyValues ty'.Parameters)
                    this.PushControlFrame(instruction, ty'.Parameters, ty'.Results)
                | Format.Else ->
                    let frame = this.PopControlFrame()
                    match frame.Instruction with
                    | Format.If _ -> this.PushControlFrame(instruction, frame.StartTypes, frame.EndTypes)
                    | _ -> raise(ElseInstructionMismatchException(index, frame.Instruction))
                | Format.End ->
                    let frame = this.PopControlFrame()
                    this.PushManyValues frame.EndTypes
                    poppedTypes <- mapToOperandTypes frame.EndTypes
                | Format.Drop -> poppedTypes <- ImmutableArray.Create(this.PopValue())
                | Format.LocalGet i ->
                    let ty = getVariableType expression i
                    this.PushValue ty
                    pushedTypes <- ImmutableArray.Create ty
                | Format.LocalSet i ->
                    let ty = getVariableType expression i
                    poppedTypes <- ImmutableArray.Create(this.PopValue ty)
                | Format.LocalTee i ->
                    let ty = getVariableType expression i
                    poppedTypes <- ImmutableArray.Create(this.PopValue ty)
                    this.PushValue ty
                    pushedTypes <- ImmutableArray.Create ty
                | Format.GlobalGet i ->
                    let ty = ValType globals[Checked.int32 i].Type.Type
                    this.PushValue ty
                    pushedTypes <- ImmutableArray.Create ty
                | Format.GlobalSet i ->
                    let glbl = globals[Checked.int32 i]
                    if glbl.Type.Mutability <> Format.Mutability.Var then raise(GlobalIsNotMutableException i)

                    let ty = ValType glbl.Type.Type
                    poppedTypes <- ImmutableArray.Create(this.PopValue ty)
                | Format.I32Load _ | Format.MemoryGrow | Format.I32Eqz ->
                    this.PopValue OperandType.i32 |> ignore
                    this.PushValue OperandType.i32
                    poppedTypes <- OperandType.singleI32
                    pushedTypes <- OperandType.singleI32
                | Format.I64Load _ ->
                    this.PopValue OperandType.i32 |> ignore
                    this.PushValue OperandType.i64
                    poppedTypes <- OperandType.singleI32
                    pushedTypes <- OperandType.singleI64
                | Format.F32Load _ ->
                    this.PopValue OperandType.i32 |> ignore
                    this.PushValue OperandType.f32
                    poppedTypes <- OperandType.singleI32
                    pushedTypes <- OperandType.singleF32
                | Format.F64Load _ ->
                    this.PopValue OperandType.i32 |> ignore
                    this.PushValue OperandType.f64
                    poppedTypes <- OperandType.singleI32
                    pushedTypes <- OperandType.singleF64
                | Format.I32Store _ -> poppedTypes <- this.PopManyValues Format.ValType.tupleI32
                | Format.I64Store _ -> poppedTypes <- this.PopManyValues Format.ValType.storeI64
                | Format.F32Store _ -> poppedTypes <- this.PopManyValues Format.ValType.storeF32
                | Format.F64Store _ -> poppedTypes <- this.PopManyValues Format.ValType.storeF64
                | Format.I32Const _ ->
                    this.PushValue OperandType.i32
                    pushedTypes <- OperandType.singleI32
                | Format.I64Const _ ->
                    this.PushValue OperandType.i64
                    pushedTypes <- OperandType.singleI64
                | Format.F32Const _ ->
                    this.PushValue OperandType.f32
                    pushedTypes <- OperandType.singleF32
                | Format.F64Const _ ->
                    this.PushValue OperandType.f64
                    pushedTypes <- OperandType.singleF64
                | Format.I32Eq | Format.I32Ne | Format.I32LtS | Format.I32LtU | Format.I32GtS | Format.I32GtU | Format.I32LeS
                | Format.I32LeU | Format.I32GeS | Format.I32GeU | Format.I32Add | Format.I32Sub | Format.I32Mul ->
                    poppedTypes <- this.PopManyValues Format.ValType.tupleI32
                    this.PushValue OperandType.i32
                    pushedTypes <- OperandType.singleI32
                | Format.I64Eq | Format.I64Ne | Format.I64LtS | Format.I64LtU | Format.I64GtS | Format.I64GtU | Format.I64LeS
                | Format.I64Eqz ->
                    this.PopValue OperandType.i64 |> ignore
                    poppedTypes <- OperandType.singleI64
                    this.PushValue OperandType.i32
                    pushedTypes <- OperandType.singleI32
                | Format.I64LeU | Format.I64GeS | Format.I64GeU ->
                    poppedTypes <- this.PopManyValues Format.ValType.tupleI64
                    this.PushValue OperandType.i32
                    pushedTypes <- OperandType.singleI32
                | Format.I64Sub | Format.I64Mul ->
                    poppedTypes <- this.PopManyValues Format.ValType.tupleI64
                    this.PushValue OperandType.i64
                    pushedTypes <- OperandType.singleI64
                | _ -> failwithf "todo %A" instruction
                
                validInstructonBuilder.Add
                    { ValidInstruction.Instruction = instruction
                      PoppedTypes = poppedTypes
                      PushedTypes = pushedTypes
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
              Element = ValueNone
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
                    else if memory.Length > 1 then multiMemoryNotSupported()
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
                    else if contents.Element.IsSome then raise(DuplicateSectionException Format.SectionId.Element)
                    else
                        order <- SectionOrder.DataCount
                        contents.Element <- ValueSome elements
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
            let moduleImportLookup =
                let imports = itemsOrEmpty contents.Imports
                let lookup = if imports.IsEmpty then null else Dictionary(capacity = imports.Length)
                for import in imports do
                    // Loop is skipped if lookup is null
                    let entries =
                        match lookup.TryGetValue import.Module with
                        | true, entries' -> entries'
                        | false, _ ->
                            let entries' = List()
                            lookup[import.Module] <- entries'
                            entries'

                    entries.Add(import)
                lookup

            if not(isNull moduleImportLookup) then
                let actualModuleImports = Dictionary(capacity = moduleImportLookup.Count)
                let mutable functions = ArrayBuilder<FunctionImport>.Create()
                let mutable tables = ArrayBuilder<TableImport>.Create()
                let mutable memories = ArrayBuilder<MemoryImport>.Create()
                let mutable globals = ArrayBuilder<GlobalImport>.Create()
                let mutable allFunctions = ArrayBuilder<_>.Create()

                for KeyValue(importModuleName, moduleImports) in moduleImportLookup do
                    functions.Clear()
                    tables.Clear()
                    memories.Clear()
                    globals.Clear()

                    for import in moduleImports do
                        match import.Description with
                        | Format.ImportDesc.Func index ->
                            let func = { FunctionImport.Name = import.Name; FunctionImport.Type = types[Checked.int32 index] }
                            functions.Add func
                            allFunctions.Add(struct(importModuleName, func))
                        | Format.ImportDesc.Table ty -> tables.Add { TableImport.Name = import.Name; TableImport.Type = ty }
                        | Format.ImportDesc.Mem limits -> memories.Add { MemoryImport.Name = import.Name; Limits = limits }
                        | Format.ImportDesc.Global ty -> globals.Add { GlobalImport.Name = import.Name; Type = ty }

                    actualModuleImports[importModuleName] <- 
                        { ModuleImports.Functions = functions.ToImmutableArray()
                          ModuleImports.Tables = tables.ToImmutableArray()
                          ModuleImports.Memories = memories.ToImmutableArray()
                          ModuleImports.Globals = globals.ToImmutableArray() }

                ModuleImportLookup(
                    actualModuleImports,
                    allFunctions.ToImmutableArray()
                )
            else
                ModuleImportLookup(null, ImmutableArray.Empty)

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
                                { ValidActiveData.Memory = Checked.int32 memory
                                  ValidActiveData.Offset = ValidExpression(
                                    offset,
                                    ImmutableArray.Empty,
                                    ImmutableArray.Empty,
                                    Format.ValType.singleI32
                                  ) } }
            Unsafe.Array.toImmutable segments

        let exports =
            let exports = ValueOption.defaultValue ImmutableArray.Empty contents.Exports
            let lookup = Dictionary(exports.Length, System.StringComparer.Ordinal)
            let memoryNames = Dictionary() // TODO: Use arrays in export lookup
            let functionNames = Dictionary()
            let tableNames = Dictionary()
            for e in exports do
                match lookup.TryGetValue e.Name with
                | true, _ -> raise(DuplicateExportException e.Name)
                | false, _ ->
                    lookup[e.Name] <-
                        match e.Description with
                        | Format.ExportDesc.Func index ->
                            functionNames.Add(Checked.int32 index, e.Name)
                            let i = Checked.int32 index - imports.Functions.Length
                            if i < 0 then
                                failwith "TODO: Fix, attempt was made to exprot a function import. Is this behavior allowed?"
                            ModuleExport.Function functions[i]
                        | Format.ExportDesc.Table index ->
                            tableNames.Add(Checked.int32 index, e.Name)
                            ModuleExport.Table
                        | Format.ExportDesc.Mem index ->
                            memoryNames.Add(Checked.int32 index, e.Name)
                            ModuleExport.Memory(Checked.int32 index)
                        | Format.ExportDesc.Global index ->
                            ModuleExport.Global
            ModuleExportLookup(memoryNames, functionNames, tableNames, lookup)

        let instructionSequenceValidator = InstructionValidator(types, memories, globals)

        for func in functions do instructionSequenceValidator.Validate func.Body

        // TODO: Check that expressions are "constant" in global values and data segments
        for glbl in globals do instructionSequenceValidator.Validate glbl.Value

        for segment in data do
            match segment.Mode with
            | ValueNone -> ()
            | ValueSome activeDataSegment -> instructionSequenceValidator.Validate activeDataSegment.Offset

        ValidModule(
            custom = contents.CustomSections.ToImmutableArray(),
            types = types,
            imports = imports,
            functions = functions,
            tables = tables,
            memories = memories,
            globals = globals,
            exports = exports,
            start = ValueOption.map Checked.int32 contents.Start,
            data = data
        )
