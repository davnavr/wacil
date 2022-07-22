/// Helper module for translation of WebAssembly imports.
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.ImportTranslator

open Wacil.Compiler
open Wacil.Compiler.Helpers.Collections

open AsmResolver.PE.DotNet.Metadata.Tables.Rows
open AsmResolver.PE.DotNet.Cil;

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types
open AsmResolver.DotNet.Code.Cil

type private ModuleClass =
    { Name: string
      Definition: TypeDefinition
      Signature: TypeSignature
      Field: FieldDefinition }

type private ParameterIndex =
    { mutable Index: uint16 }

    member this.Reset() = this.Index <- 1us

    member this.Next() =
        let index = this.Index
        this.Index <- Checked.(+) this.Index 1us
        index

let translateModuleImports
    (mangleMemberName: string -> string)
    (delegateTypeCache: MethodSignature -> _)
    (translateFuncType: Wasm.Format.FuncType -> MethodSignature)
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
        importParameterIndex.Reset()

        let imports = wasm.Imports[moduleImportName]
        let mangledModuleImportName = mangleMemberName moduleImportName

        let importClassDefinition =
            DefinitionHelpers.addNormalClass
                syslib
                moduleClassDefinition.Module
                (TypeAttributes.Sealed ||| TypeAttributes.Public)
                ns
                mangledModuleImportName

        let importClassSignature = TypeDefOrRefSignature importClassDefinition

        let importInstanceField =
            DefinitionHelpers.addFieldDefinition
                moduleClassDefinition
                (FieldSignature importClassSignature)
                FieldAttributes.InitOnly
                mangledModuleImportName

        for func in imports.Functions do
            let name = mangleMemberName func.Name
            let translatedFunctionSignature = translateFuncType func.Type
            let functionDelegateType: DelegateCache.Instantiation = delegateTypeCache translatedFunctionSignature

            let field =
                DefinitionHelpers.addFieldDefinition
                    importClassDefinition
                    functionDelegateType.FieldSignature
                    FieldAttributes.InitOnly
                    name

            ()

        // TODO: table imports

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

            members.Memories[int32 memory.Index] <- MemoryMember.Imported(importInstanceField, field)

        // TODO: Global imports

        let importConstructorDefinition =
            DefinitionHelpers.addInstanceConstructor
                (constructorParameterTypes.ToArray())
                (MethodAttributes.HideBySig ||| MethodAttributes.Public)
                importClassDefinition

        for index in 1..constructorParameterNames.Count do
            let name = constructorParameterNames[index - 1]
            importConstructorDefinition.ParameterDefinitions.Add(ParameterDefinition(uint16 index, name, Unchecked.defaultof<_>))

        do
            importConstructorDefinition.CilMethodBody <- CilMethodBody importConstructorDefinition
            let il = importConstructorDefinition.CilMethodBody.Instructions
            CilHelpers.emitObjectCtorCall syslib il
            for init in importMemberInitializers do init il
            il.Add(CilInstruction CilOpCodes.Ret)
        
        translatedModuleImports.Add
            { Name = mangledModuleImportName
              Definition = importClassDefinition
              Signature = importClassSignature
              Field = importInstanceField }

    let moduleClassConstructor =
        let imports = translatedModuleImports.ToImmutableArray()
        let mutable parameters = ArrayBuilder.Create imports.Length

        for i in imports do parameters.Add i.Signature

        let constructor =
            DefinitionHelpers.addInstanceConstructor
                (parameters.ToArray())
                (MethodAttributes.Public ||| MethodAttributes.HideBySig)
                moduleClassDefinition

        for sequence in 1..imports.Length do
            let name = imports[sequence - 1].Name
            constructor.ParameterDefinitions.Add(ParameterDefinition(uint16 sequence, name, Unchecked.defaultof<_>))

        constructor.CilMethodBody <- CilMethodBody constructor

        let il = constructor.CilMethodBody.Instructions
        CilHelpers.emitObjectCtorCall syslib il
        for sequence in 1..imports.Length do
            let i = imports[sequence - 1]
            CilHelpers.emitArgumentStoreWithNullCheck syslib (uint16 sequence) i.Name i.Field il
            
        constructor

    moduleClassConstructor
