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

    let outputModuleName = String.defaultValue "module" options.OutputName
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

    let rtlib = RuntimeLibrary.importTypes options.RuntimeVersion translateValType syslib mdle

    let implementationDetailsClass =
        DefinitionHelpers.addNormalClass syslib mdle TypeAttributes.Sealed String.empty "<PrivateImplementationDetails>"

    let mangleMemberName = NameMangling.mangle (System.Text.StringBuilder())

    // TODO: Check that namespace name is correct (.Split('.') should not contain empty strings)
    let mainClassNamespace = String.defaultValue outputModuleName options.Namespace

    let mainClassDefinition =
        mangleMemberName options.MainClassName
        |> String.defaultValue outputModuleName
        |> DefinitionHelpers.addNormalClass syslib mdle (TypeAttributes.Sealed ||| TypeAttributes.Public) mainClassNamespace
        
    let mainClassSignature = TypeDefOrRefSignature mainClassDefinition

    let members =
        { Functions = Array.zeroCreate(input.Imports.Imports.Functions.Length + input.Functions.Length)
          //Tables =
          Memories = Array.zeroCreate(input.Imports.Imports.Memories.Length + input.Memories.Length)
          Globals = Array.zeroCreate(input.Imports.Imports.Globals.Length + input.Globals.Length) }

    let tupleTypeCache = TupleCache.create mdle mscorlib translateValType

    let translateFuncType (ty: FuncType) =
        let firstReturnType, extraParameterCount =
            if ty.Results.IsEmpty then
                mdle.CorLibTypeFactory.Void :> TypeSignature, 0
            else
                translateValType ty.Results[0], 1

        let mutable parameterTypes = ArrayBuilder<TypeSignature>.Create(ty.Parameters.Length + extraParameterCount)

        for parameter in ty.Parameters do
            parameterTypes.Add(translateValType parameter)

        if ty.Results.Length > 1 then
            let remainingReturnTypes =
                if ty.Results.Length > 2 then
                    tupleTypeCache(ty.Results.AsMemory().Slice(1)).Signature
                else
                    translateValType ty.Results[1]
            parameterTypes.Add(remainingReturnTypes.MakeByReferenceType())

        MethodSignature(CallingConventionAttributes.HasThis, firstReturnType, parameterTypes.ToImmutableArray())

    let delegateTypeCache = DelegateCache.create mdle mscorlib

    let mainInstanceConstructor =
        ImportTranslator.translateModuleImports
            mangleMemberName
            delegateTypeCache
            translateFuncType
            syslib
            rtlib
            mainClassDefinition
            input
            mainClassNamespace
            members

    MemoryTranslator.translateModuleMemories
        mangleMemberName
        rtlib
        mainClassDefinition
        input
        members
        mainInstanceConstructor.CilMethodBody

    GlobalTranslator.translateGlobalVariables
        mangleMemberName
        translateValType
        rtlib
        mainClassDefinition
        mainClassSignature
        input
        members
        mainInstanceConstructor.CilMethodBody

    // TODO: Emit calls to global initializers in mainInstanceConstructor
    // TODO: Emit calls to the things with element segments

    // TODO: Generate function definitions

    mainInstanceConstructor.CilMethodBody.Instructions.Add(CilInstruction CilOpCodes.Ret)

    mdle

let compileToStream options input (stream: System.IO.Stream) =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanWrite then invalidArg (nameof stream) "Destination stream must support writing"
        (compileToModuleDefinition options input).Write stream
    finally
        stream.Close()
