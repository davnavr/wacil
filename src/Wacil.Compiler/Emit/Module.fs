module Wacil.Compiler.Emit.Module

open AsmResolver.DotNet

open Wacil.Compiler.Helpers

open Wacil.Compiler.Wasm.Validation

let compileToStream (options: Options) (input: ValidModule) (stream: System.IO.Stream) =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanWrite then invalidArg (nameof stream) "Destination stream must support writing"

        let outputName = String.defaultValue "module" options.Name
        
        let moduleDefinition =
            let coreLibraryReference =
                match options.TargetFramework with
                | TargetFramework.Net6 -> KnownCorLibs.SystemRuntime_v6_0_0_0

            new ModuleDefinition(outputName + ".dll", coreLibraryReference)

        moduleDefinition.Write stream
    finally
        stream.Close()
