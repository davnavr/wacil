module Wacil.Compiler.Emit.Module

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open AsmResolver
open AsmResolver.PE.DotNet.Metadata.Tables.Rows;

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types

open AsmResolver.PE.DotNet.Cil
open AsmResolver.DotNet.Code.Cil

open Wacil.Compiler.Helpers
open Wacil.Compiler.Helpers.Collections

open Wacil.Compiler.Wasm.Format
open Wacil.Compiler.Wasm.Validation

type CilInstruction with
    static member CreateLdloc index =
        match index with
        | 0 -> CilInstruction CilOpCodes.Ldloc_0
        | 1 -> CilInstruction CilOpCodes.Ldloc_1
        | 2 -> CilInstruction CilOpCodes.Ldloc_2
        | 3 -> CilInstruction CilOpCodes.Ldloc_3
        | _ when index < 0 -> raise(System.ArgumentOutOfRangeException(nameof index))
        | _ when index <= 255 -> CilInstruction(CilOpCodes.Ldloc_S, uint8 index)
        | _ -> CilInstruction(CilOpCodes.Ldloc, Checked.uint16 index)

    static member CreateLdarg index =
        match index with
        | 0 -> CilInstruction CilOpCodes.Ldarg_0
        | 1 -> CilInstruction CilOpCodes.Ldarg_1
        | 2 -> CilInstruction CilOpCodes.Ldarg_2
        | 3 -> CilInstruction CilOpCodes.Ldarg_3
        | _ when index < 0 -> raise(System.ArgumentOutOfRangeException(nameof index))
        | _ when index <= 255 -> CilInstruction(CilOpCodes.Ldarg_S, uint8 index)
        | _ -> CilInstruction(CilOpCodes.Ldarg, Checked.uint16 index)

    static member CreateStloc index =
        match index with
        | 0 -> CilInstruction CilOpCodes.Stloc_0
        | 1 -> CilInstruction CilOpCodes.Stloc_1
        | 2 -> CilInstruction CilOpCodes.Stloc_2
        | 3 -> CilInstruction CilOpCodes.Stloc_3
        | _ when index < 0 -> raise(System.ArgumentOutOfRangeException(nameof index))
        | _ when index <= 255 -> CilInstruction(CilOpCodes.Stloc_S, uint8 index)
        | _ -> CilInstruction(CilOpCodes.Stloc, Checked.uint16 index)

    static member CreateStarg index =
        match index with
        | _ when index < 0 -> raise(System.ArgumentOutOfRangeException(nameof index))
        | _ when index <= 255 -> CilInstruction(CilOpCodes.Starg_S, uint8 index)
        | _ -> CilInstruction(CilOpCodes.Starg, Checked.uint16 index)

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type LocalIndex =
    | Arg of a: int32
    | Loc of int32

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type TranslatedGlobal =
    { Field: FieldDefinition
      Initializer: MethodDefinition
      Setter: MethodDefinition voption }

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type TranslatedTable =
    { ElementType: RefType
      Field: FieldDefinition
      Operations: RuntimeLibrary.TableInstantiation }

[<NoComparison; NoEquality>]
type TranslatedFunctionImport =
    { Delegate: TypeDefinition
      Field: FieldDefinition
      Invoke: MethodDefinition }

[<NoComparison; NoEquality>]
type ModuleImport =
    { Class: TypeDefinition
      Signature: TypeDefOrRefSignature
      Field: FieldDefinition
      Functions: ImmutableArray<TranslatedFunctionImport> }

[<RequireQualifiedAccess; NoComparison; ReferenceEquality>]
type BranchTarget =
    | Loop of CilInstructionLabel
    | Block of CilInstructionLabel
    | If of elseBranchLabel: CilInstructionLabel * endBranchLabel: CilInstructionLabel

[<IsByRefLike; Struct; NoComparison; NoEquality>]
type BranchTargetStack =
    { mutable Targets: ArrayBuilder<BranchTarget> }

    member this.PushBlock() =
        this.Targets.Add(BranchTarget.Block(CilInstructionLabel()))

    member this.PushLoop start =
        this.Targets.Add(BranchTarget.Loop(CilInstructionLabel start))

    member this.PushIf(): CilInstructionLabel =
        let elseBranchTarget = CilInstructionLabel()
        this.Targets.Add(BranchTarget.If(elseBranchTarget, CilInstructionLabel()))
        elseBranchTarget

    member this.Pop() =
        let target = this.Targets.Pop()
        target

    member this.GetLabel (LabelIdx target): CilInstructionLabel =
        match this.Targets.ItemFromEnd target with
        | BranchTarget.Block label
        | BranchTarget.Loop label
        | BranchTarget.If(_, label) -> label

let methodImplAggressiveInlining: MethodImplAttributes = LanguagePrimitives.EnumOfValue 256us

let compileToModuleDefinition (options: Options) (input: ValidModule) =
    let mscorlib =
        match options.TargetFramework with
        | TargetFramework.Net6 -> KnownCorLibs.SystemRuntime_v6_0_0_0

    let strbuf = System.Text.StringBuilder()
    let outputModuleName = String.defaultValue "module" options.Name
    let mdle = new ModuleDefinition(outputModuleName + ".dll", mscorlib)

    let syslib = SystemLibrary.importTypes mscorlib mdle

    mdle.GetOrCreateModuleType().BaseType <- syslib.Object.Type

    let assembly =
        match options.OutputType with
        | OutputType.Assembly version ->
            let assembly = new AssemblyDefinition(outputModuleName, version)
            assembly.Modules.Add mdle

            assembly.CustomAttributes.Add(
                CustomAttribute(
                    syslib.TargetFrameworkAttributeConstructor,
                    CustomAttributeSignature(
                        Array.singleton(CustomAttributeArgument(
                            mdle.CorLibTypeFactory.String,
                            options.TargetFramework.FrameworkName
                        )),
                        Array.singleton(CustomAttributeNamedArgument(
                            CustomAttributeArgumentMemberType.Field,
                            Utf8String "FrameworkDisplayName",
                            mdle.CorLibTypeFactory.String,
                            CustomAttributeArgument(mdle.CorLibTypeFactory.String, String.empty)
                        ))
                    )
                )
            )

            ValueSome assembly
        | OutputType.Module ->
            ValueNone

    let translateValType =
        let coreLibraryTypes = mdle.CorLibTypeFactory
        function
        | ValType.Num I32 -> coreLibraryTypes.Int32 :> TypeSignature
        | ValType.Num I64 -> coreLibraryTypes.Int64
        | ValType.Num F32 -> coreLibraryTypes.Single
        | ValType.Num F64 -> coreLibraryTypes.Double
        | ValType.Ref ExternRef -> coreLibraryTypes.Object
        | ValType.Ref FuncRef -> syslib.MulticastDelegate.Signature
        | ValType.Vec _ -> raise(System.NotImplementedException "TODO: conversion of vectors to System.Runtime.Intrinsics.Vector128")

    let getFuncTypeSignature cconv (ty: FuncType) =
        // TODO: For multi-return, use out parameters
        if ty.Results.Length > 1 then failwith "TODO: Compilation of functions with multiple return values is not yet supported"

        let returnTypes =
            if ty.Results.IsEmpty
            then mdle.CorLibTypeFactory.Void :> TypeSignature
            else translateValType ty.Results[0]

        let mutable parameterTypes = ArrayBuilder<TypeSignature>.Create(ty.Parameters.Length)

        for parameter in ty.Parameters do
            parameterTypes.Add(translateValType parameter)

        MethodSignature(cconv, returnTypes, parameterTypes.ToImmutableArray())

    let rtlib = RuntimeLibrary.importTypes options.RuntimeVersion translateValType syslib mdle

    let implementationDetailsClass =
        let definition = TypeDefinition(String.empty, "<PrivateImplementationDetails>", TypeAttributes.Sealed)
        definition.BaseType <- syslib.Object.Type
        mdle.TopLevelTypes.Add definition
        definition
        
    

    mdle

let compileToStream options input (stream: System.IO.Stream) =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanWrite then invalidArg (nameof stream) "Destination stream must support writing"
        (compileToModuleDefinition options input).Write stream
    finally
        stream.Close()
