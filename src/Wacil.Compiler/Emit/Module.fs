module Wacil.Compiler.Emit.Module

open AsmResolver
open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures

open Wacil.Compiler.Helpers

open Wacil.Compiler.Wasm.Validation

let compileToStream (options: Options) (input: ValidModule) (stream: System.IO.Stream) =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanWrite then invalidArg (nameof stream) "Destination stream must support writing"

        let coreLibraryReference =
            match options.TargetFramework with
            | TargetFramework.Net6 -> KnownCorLibs.SystemRuntime_v6_0_0_0

        let outputName = String.defaultValue "module" options.Name
        let moduleDefinition = new ModuleDefinition(outputName + ".dll", coreLibraryReference)

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
                            CallingConventionAttributes.Default,
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

        moduleDefinition.Write stream
    finally
        stream.Close()
