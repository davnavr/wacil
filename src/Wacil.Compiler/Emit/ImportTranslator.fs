/// Helper module for translation of WebAssembly imports.
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.ImportTranslator

open Wacil.Compiler
open Wacil.Compiler.Helpers.Collections

open AsmResolver.PE.DotNet.Metadata.Tables.Rows;

open AsmResolver.DotNet

/// Represents a generated .NET class corresponding to a WebAssembly module import.
type ModuleClass =
    { Definition: TypeDefinition
      //Signature:
      }

let translateModuleImports
    (mangleMemberName: string -> string)
    (syslib: SystemLibrary.References)
    (rtlib: RuntimeLibrary.References)
    (moduleClassDefinition: TypeDefinition)
    (wasm: Wasm.Validation.ValidModule)
    (ns: string)
    (members: ModuleMembers)
    =
    let mutable constructorParameterTypes = ArrayBuilder.Create()
    //let mutable importedMemoryMembers = ArrayBuilder<MemoryMembers>

    for moduleImportName in wasm.Imports.Modules do
        constructorParameterTypes.Clear()

        let imports = wasm.Imports[moduleImportName]

        let importClassDefinition =
            DefinitionHelpers.addNormalClass
                syslib
                moduleClassDefinition.Module
                (TypeAttributes.Sealed ||| TypeAttributes.Public)
                ns
                (mangleMemberName moduleImportName)

        // TODO: Add parameter types

        // TODO: First, are the function imports, followed by the table imports

        // TODO: Module import time

        let importClassConstructor =
            DefinitionHelpers.addInstanceConstructor
                (constructorParameterTypes.ToArray())
                (MethodAttributes.HideBySig ||| MethodAttributes.Public)
                importClassDefinition

        ()

    failwith "AA" // importedMemoryMembers.ToArray()
