/// Helper module for translation of WebAssembly imports.
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.ImportTranslator

open Wacil.Compiler

open AsmResolver.PE.DotNet.Metadata.Tables.Rows;

open AsmResolver.DotNet

/// Represents a generated .NET class corresponding to a WebAssembly module import.
type ModuleClass =
    { Definition: TypeDefinition }

let translateModuleImports
    (mangleMemberName: string -> string)
    (syslib: SystemLibrary.References)
    (rtlib: RuntimeLibrary.References)
    (moduleClassDefinition: TypeDefinition)
    (wasm: Wasm.Validation.ValidModule)
    (ns: string)
    =
    //let mutable importedMemoryMembers = ArrayBuilder<MemoryMembers>

    for moduleImportName in wasm.Imports.Modules do
        let imports = wasm.Imports[moduleImportName]

        let moduleClassDefinition =
            TypeDefinition(
                ns,
                failwith "TODO: Mangle import name",
                TypeAttributes.Sealed ||| TypeAttributes.Public
            )

        ()

    failwith "AA"
