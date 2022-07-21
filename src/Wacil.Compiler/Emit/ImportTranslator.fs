/// Helper module for translation of WebAssembly imports.
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.ImportTranslator

open Wacil.Compiler
open Wacil.Compiler.Helpers.Collections

open AsmResolver.PE.DotNet.Metadata.Tables.Rows
open AsmResolver.PE.DotNet.Cil;

open AsmResolver.DotNet
open AsmResolver.DotNet.Code.Cil

/// Represents a generated .NET class corresponding to a WebAssembly module import.
type ModuleClass =
    { Definition: TypeDefinition
      //Signature:
      }

type private ParameterIndex =
    { mutable Index: uint16 }

    member this.Next() =
        let index = this.Index
        this.Index <- Checked.(+) this.Index 1us
        index

let translateModuleImports
    (mangleMemberName: string -> string)
    (syslib: SystemLibrary.References)
    (rtlib: RuntimeLibrary.References)
    (moduleClassDefinition: TypeDefinition)
    (wasm: Wasm.Validation.ValidModule)
    (ns: string)
    (members: ModuleMembers)
    =
    let constructorParameterNames = ResizeArray()
    let mutable constructorParameterTypes = ArrayBuilder.Create()
    let mutable translatedModuleImports = ArrayBuilder<ModuleClass>.Create(wasm.Imports.Modules.Count)

    let importMemberInitializers = ResizeArray<CilInstructionCollection -> unit>()
    let importParameterIndex = { Index = 1us }

    for moduleImportName in wasm.Imports.Modules do
        constructorParameterTypes.Clear()
        constructorParameterNames.Clear()
        importMemberInitializers.Clear()
        importParameterIndex.Index <- 1us

        let imports = wasm.Imports[moduleImportName]

        let importClassDefinition =
            DefinitionHelpers.addNormalClass
                syslib
                moduleClassDefinition.Module
                (TypeAttributes.Sealed ||| TypeAttributes.Public)
                ns
                (mangleMemberName moduleImportName)

        // TODO: First, are the function imports, followed by the table imports

        for memory in imports.Memories do
            let name = mangleMemberName memory.Name

            let field =
                DefinitionHelpers.addFieldDefinition
                    importClassDefinition
                    rtlib.Memory.FieldSignature
                    FieldAttributes.InitOnly
                    name
                    
            constructorParameterTypes.Add rtlib.Memory.Signature
            constructorParameterNames.Add name

            let index = importParameterIndex.Next()

            importMemberInitializers.Add(CilHelpers.emitArgumentStoreWithNullCheck syslib index name field)

        let importConstructorDefinition =
            DefinitionHelpers.addInstanceConstructor
                (constructorParameterTypes.ToArray())
                (MethodAttributes.HideBySig ||| MethodAttributes.Public)
                importClassDefinition

        for index in 1..constructorParameterNames.Count do
            let name = constructorParameterNames[index - 1]
            importConstructorDefinition.ParameterDefinitions.Add(ParameterDefinition(uint16 index, name, Unchecked.defaultof<_>))

        do
            let body = CilMethodBody importConstructorDefinition
            let il = body.Instructions
            il.Add(CilInstruction CilOpCodes.Ldarg_0)
            il.Add(CilInstruction(CilOpCodes.Call, syslib.Object.Constructor))
            for init in importMemberInitializers do init il
            il.Add(CilInstruction CilOpCodes.Ret)
            importConstructorDefinition.CilMethodBody <- body
        
        translatedModuleImports.Add
            { Definition = importClassDefinition }

    //TODO: Also return some closure that appends code to .ctor that assigns to the field corresponding to the module import
    translatedModuleImports.ToImmutableArray()
