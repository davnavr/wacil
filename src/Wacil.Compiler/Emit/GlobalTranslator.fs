/// Helper module for translation of WebAssembly global variables.
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.GlobalTranslator

open Wacil.Compiler
open Wacil.Compiler.Helpers.Collections

open AsmResolver.PE.DotNet.Metadata.Tables.Rows
open AsmResolver.PE.DotNet.Cil;

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types
open AsmResolver.DotNet.Code.Cil

let translateGlobalVariables
    mangleMemberName
    translateValType
    (rtlib: RuntimeLibrary.References)
    (moduleClassDefinition: TypeDefinition)
    (mainClassSignature: TypeDefOrRefSignature)
    (wasm: Wasm.Validation.ValidModule)
    (members: ModuleMembers)
    (moduleInstanceConstructor: CilMethodBody)
    =
    let firstDefinedIndex = wasm.Imports.Imports.Globals.Length

    for i in 0..wasm.Memories.Length - 1 do
        let glbl = wasm.Globals[i]
        let index = firstDefinedIndex + i
        let memoryFieldName = "__global@" + string index

        let isMutableGlobal =
            match glbl.Type.Mutability with
            | Wasm.Format.Mutability.Const -> false
            | Wasm.Format.Mutability.Var -> true

        //let initialValueMethod = // TODO: Generate a method that produces the inital value

        match wasm.Exports.GetGlobalName(Wasm.Format.GlobalIdx index) with
        | true, globalExportName ->
            let globalTypeInstantiation = rtlib.InstantiatedGlobal glbl.Type.Type

            let field =
                DefinitionHelpers.addFieldDefinition
                    moduleClassDefinition
                    globalTypeInstantiation.FieldSignature
                    FieldAttributes.InitOnly
                    memoryFieldName

            DefinitionHelpers.addInstanceFieldGetter
                moduleClassDefinition
                globalTypeInstantiation.Instantiation
                PropertyAttributes.None
                (MethodAttributes.Public ||| MethodAttributes.HideBySig)
                field
                (mangleMemberName globalExportName)

            members.Globals[index] <- GlobalMember.DefinedExport field
            
            let il = moduleInstanceConstructor.Instructions
            failwith "TODO: Other initialization stuff for exported global"
        | false, _ ->
            let translatedGlobalType = translateValType glbl.Type.Type

            let field =
                DefinitionHelpers.addFieldDefinition
                    moduleClassDefinition
                    (FieldSignature translatedGlobalType)
                    (if isMutableGlobal then FieldAttributes.PrivateScope else FieldAttributes.InitOnly)
                    memoryFieldName

            let setter =
                if isMutableGlobal then
                    let signature = MethodSignature(
                        CallingConventionAttributes.Default,
                        moduleClassDefinition.Module.CorLibTypeFactory.Void,
                        [| translatedGlobalType; mainClassSignature |]
                    )

                    DefinitionHelpers.addMethodDefinition
                        moduleClassDefinition
                        signature
                        MethodAttributes.Static
                        ("__global_set@" + string index)
                    |> ValueSome
                else
                    ValueNone

            members.Globals[index] <- GlobalMember.Defined(field, setter)

            let il = moduleInstanceConstructor.Instructions
            failwith "TODO: Generate setter and other stuff for non exported global"
