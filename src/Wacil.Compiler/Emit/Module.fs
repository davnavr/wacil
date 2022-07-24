module Wacil.Compiler.Emit.Module

open AsmResolver
open AsmResolver.PE.DotNet.Metadata.Tables.Rows;

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types

open AsmResolver.DotNet.Code.Cil
open AsmResolver.PE.DotNet.Cil

open Wacil.Compiler.Helpers
open Wacil.Compiler.Helpers.Collections

open Wacil.Compiler.Wasm.Format
open Wacil.Compiler.Wasm.Validation

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
          Tables = Array.zeroCreate(input.Imports.Imports.Tables.Length + input.Tables.Length)
          Memories = Array.zeroCreate(input.Imports.Imports.Memories.Length + input.Memories.Length)
          Globals = Array.zeroCreate(input.Imports.Imports.Globals.Length + input.Globals.Length)
          DataSegments = Array.zeroCreate input.Data.Length }

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

    let webAssemblyExpressions = ResizeArray<Transpiler.Input>(input.Globals.Length + input.Functions.Length)

    let mainInstanceConstructor =
        ImportTranslator.translateModuleImports
            mangleMemberName
            delegateTypeCache
            translateFuncType
            syslib
            rtlib
            mainClassDefinition
            mainClassSignature
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

    TableMember.translateModuleTables
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
        webAssemblyExpressions
        mainInstanceConstructor.CilMethodBody

    FunctionTranslator.translateFunctionDefinitions
        mangleMemberName
        translateFuncType
        rtlib
        mainClassDefinition
        mainClassSignature
        input
        members
        webAssemblyExpressions

    let mainStaticInitializer =
        DefinitionHelpers.addMethodDefinition
            mainClassDefinition
            (MethodSignature(CallingConventionAttributes.Default, mdle.CorLibTypeFactory.Void, Seq.empty))
            (MethodAttributes.Static ||| MethodAttributes.RuntimeSpecialName ||| MethodAttributes.SpecialName)
            ".cctor"

    mainStaticInitializer.CilMethodBody <- CilMethodBody mainStaticInitializer

    // TODO: Emit calls in the constructor to the element segment things

    let constantByteFactory = ConstantBytes.createFieldFactory implementationDetailsClass syslib

    // Translates the module data segments
    // Note that this depends on the memories already being translated, as active data segments copy themselves to a memory
    DataSegmentMember.translateDataSegments
        constantByteFactory
        syslib
        rtlib
        mainClassDefinition
        input.Data
        members
        webAssemblyExpressions
        mainInstanceConstructor.CilMethodBody
        mainStaticInitializer.CilMethodBody

    Transpiler.translateWebAssembly translateValType rtlib members webAssemblyExpressions

    // Generate call to the start function after all initialization has been done
    match input.Start with
    | ValueSome(FuncIdx start) ->
        let il = mainInstanceConstructor.CilMethodBody.Instructions
        il.Add(CilInstruction CilOpCodes.Ldarg_0)

        match members.Functions[start] with
        | FunctionMember.Defined(invoke, _, _)
        | FunctionMember.Imported(_, _, _, invoke, _) -> il.Add(CilInstruction(CilOpCodes.Call, invoke))
    | ValueNone -> ()

    mainInstanceConstructor.CilMethodBody.Instructions.Add(CilInstruction CilOpCodes.Ret)
    mainStaticInitializer.CilMethodBody.Instructions.Add(CilInstruction CilOpCodes.Ret)

    mdle

let compileToStream options input (stream: System.IO.Stream) =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanWrite then invalidArg (nameof stream) "Destination stream must support writing"
        (compileToModuleDefinition options input).Write stream
    finally
        stream.Close()
