module Wacil.Compiler.Emit.Module

open Microsoft.FSharp.Core.Printf

open AsmResolver
open AsmResolver.PE.DotNet.Metadata.Tables.Rows;

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types

open AsmResolver.PE.DotNet.Cil
open AsmResolver.DotNet.Code.Cil

open Wacil.Compiler.Helpers

open Wacil.Compiler.Wasm.Validation

/// <summary>Represents the references to the Wacil runtime library (<c>Wacil.Runtime.dll</c>).</summary>
[<NoComparison; NoEquality>]
type RuntimeLibraryReference =
    { Memory: ITypeDefOrRef
      MemoryConstructor: IMethodDefOrRef }

let compileToModuleDefinition (options: Options) (input: ValidModule) =
    let coreLibraryReference =
        match options.TargetFramework with
        | TargetFramework.Net6 -> KnownCorLibs.SystemRuntime_v6_0_0_0

    let stringBuffer = System.Text.StringBuilder()
    let outputName = String.defaultValue "module" options.Name
    let moduleDefinition = new ModuleDefinition(outputName + ".dll", coreLibraryReference)

    let coreSystemObject =
        coreLibraryReference.CreateTypeReference("System", "Object")
        |> moduleDefinition.DefaultImporter.ImportTypeOrNull

    let assemblyDefinition =
        match options.OutputType with
        | OutputType.Assembly ->
            let assembly = new AssemblyDefinition(outputName, options.Version)
            assembly.Modules.Add moduleDefinition

            let tfmAttributeClass =
                coreLibraryReference.CreateTypeReference(
                    "System.Runtime.Versioning",
                    "TargetFrameworkAttribute"
                )
                |> moduleDefinition.DefaultImporter.ImportTypeOrNull

            let tfmAttributeConstructor =
                tfmAttributeClass.CreateMemberReference(
                    ".ctor",
                    new MethodSignature(
                        CallingConventionAttributes.HasThis,
                        moduleDefinition.CorLibTypeFactory.Void,
                        [| moduleDefinition.CorLibTypeFactory.String |]
                    )
                )
                |> moduleDefinition.DefaultImporter.ImportMethod

            assembly.CustomAttributes.Add(
                CustomAttribute(
                    tfmAttributeConstructor :?> ICustomAttributeType,
                    CustomAttributeSignature(
                        Array.singleton(CustomAttributeArgument(
                            moduleDefinition.CorLibTypeFactory.String,
                            options.TargetFramework.FrameworkName
                        )),
                        Array.singleton(CustomAttributeNamedArgument(
                            CustomAttributeArgumentMemberType.Field,
                            Utf8String "FrameworkDisplayName",
                            moduleDefinition.CorLibTypeFactory.String,
                            CustomAttributeArgument(moduleDefinition.CorLibTypeFactory.String, String.empty)
                        ))
                    )
                )
            )

            ValueSome assembly
        | OutputType.Module ->
            ValueNone

    let runtimeLibraryReference =
        let runtimeLibraryName = "Wacil.Runtime"
        let assembly = AssemblyReference(runtimeLibraryName, options.RuntimeVersion)
        moduleDefinition.AssemblyReferences.Add assembly

        let runtimeMemoryClass =
            assembly.CreateTypeReference(runtimeLibraryName, "Memory") |> moduleDefinition.DefaultImporter.ImportTypeOrNull

        let runtimeMemoryConstructor =
            runtimeMemoryClass.CreateMemberReference(
                ".ctor",
                new MethodSignature(
                    CallingConventionAttributes.HasThis,
                    moduleDefinition.CorLibTypeFactory.Void,
                    [|
                        moduleDefinition.CorLibTypeFactory.Int32
                        moduleDefinition.CorLibTypeFactory.Int32
                    |]
                )
            )
            |> moduleDefinition.DefaultImporter.ImportMethodOrNull

        { Memory = runtimeMemoryClass
          MemoryConstructor = runtimeMemoryConstructor }

    moduleDefinition.GetOrCreateModuleType().BaseType <- coreSystemObject

    let classDefinition =
        TypeDefinition(
            String.orEmpty options.Namespace,
            outputName,
            TypeAttributes.Sealed ||| TypeAttributes.Public ||| TypeAttributes.SequentialLayout
        )

    classDefinition.BaseType <- coreSystemObject
    moduleDefinition.TopLevelTypes.Add classDefinition

    let classDefinitionConstructor =
        // TODO: Signature should accept exports
        MethodDefinition(
            ".ctor",
            MethodAttributes.Public ||| MethodAttributes.RuntimeSpecialName ||| MethodAttributes.SpecialName |||
            MethodAttributes.HideBySig,
            MethodSignature(
                CallingConventionAttributes.HasThis,
                moduleDefinition.CorLibTypeFactory.Void,
                [||]
            )
        )

    classDefinition.Methods.Add classDefinitionConstructor

    let classConstructorBody =
        let coreSystemObjectConstructor =
            coreSystemObject.CreateMemberReference(
                ".ctor",
                new MethodSignature(
                    CallingConventionAttributes.HasThis,
                    moduleDefinition.CorLibTypeFactory.Void,
                    Array.empty
                )
            )
            |> moduleDefinition.DefaultImporter.ImportMethod

        let body = CilMethodBody classDefinitionConstructor
        let instructons = body.Instructions
        instructons.Add(CilInstruction CilOpCodes.Ldarg_0)
        instructons.Add(CilInstruction(CilOpCodes.Call, coreSystemObjectConstructor))
        body

    for i = 0 to input.Memories.Length - 1 do
        let memory = input.Memories[i]
        let memoryField =
            FieldDefinition(
                stringBuffer.Clear().Append("memory#").Append(i).ToString(),
                FieldAttributes.InitOnly,
                FieldSignature(TypeDefOrRefSignature runtimeLibraryReference.Memory)
            )

        classDefinition.Fields.Add memoryField

        let instructions = classConstructorBody.Instructions
        instructions.Add(CilInstruction CilOpCodes.Ldarg_0)
        instructions.Add(CilInstruction.CreateLdcI4(Checked.int32 memory.Minimum))
        instructions.Add(CilInstruction.CreateLdcI4(ValueOption.map Checked.int32 memory.Maximum |> ValueOption.defaultValue -1))
        instructions.Add(CilInstruction(CilOpCodes.Newobj, runtimeLibraryReference.MemoryConstructor))
        instructions.Add(CilInstruction(CilOpCodes.Stfld, memoryField))

        match input.Exports.GetMemoryName(Checked.uint32 i) with
        | true, name ->
            let accessor =
                MethodDefinition(
                    stringBuffer.Clear().Append("get_").Append(name).ToString(),
                    MethodAttributes.Public ||| //MethodAttributes.RuntimeSpecialName ||| MethodAttributes.SpecialName |||
                    MethodAttributes.HideBySig,
                    MethodSignature(
                        CallingConventionAttributes.HasThis,
                        TypeDefOrRefSignature runtimeLibraryReference.Memory,
                        Array.empty
                    )
                )
            
            classDefinition.Methods.Add accessor

            let body = CilMethodBody accessor
            let instructions = body.Instructions
            instructions.Add(CilInstruction CilOpCodes.Ldarg_0)
            instructions.Add(CilInstruction(CilOpCodes.Ldfld, memoryField))
            instructions.Add(CilInstruction CilOpCodes.Ret)
            accessor.CilMethodBody <- body

            // TODO: Generate accessor
            ()
        | false, _ -> ()

    // Done generating code for constructor
    classConstructorBody.Instructions.Add(CilInstruction CilOpCodes.Ret)
    classDefinitionConstructor.CilMethodBody <- classConstructorBody

    moduleDefinition

let compileToStream options input (stream: System.IO.Stream) =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanWrite then invalidArg (nameof stream) "Destination stream must support writing"
        (compileToModuleDefinition options input).Write stream
    finally
        stream.Close()
