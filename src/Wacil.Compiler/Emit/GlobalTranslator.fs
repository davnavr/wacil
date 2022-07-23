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
    (translateValType: _ -> TypeSignature)
    (rtlib: RuntimeLibrary.References)
    (moduleClassDefinition: TypeDefinition)
    (mainClassSignature: TypeDefOrRefSignature)
    (wasm: Wasm.Validation.ValidModule)
    (members: ModuleMembers)
    (transpilerInputBuilder: ResizeArray<Transpiler.Input>)
    (moduleInstanceConstructor: CilMethodBody)
    =
    let firstDefinedIndex = wasm.Imports.Imports.Globals.Length

    for i in 0..wasm.Globals.Length - 1 do
        let glbl = wasm.Globals[i]
        let index = firstDefinedIndex + i
        let globalIndexString = string index
        let globalFieldName = "__global@" + globalIndexString

        let isMutableGlobal =
            match glbl.Type.Mutability with
            | Wasm.Format.Mutability.Const -> false
            | Wasm.Format.Mutability.Var -> true

        let translatedGlobalType = translateValType glbl.Type.Type

        let initialValueMethod =
            // TODO: Keep a list somewhere that keeps track of which WASM expressions need to be translated
            DefinitionHelpers.addMethodDefinition
                moduleClassDefinition
                (MethodSignature(CallingConventionAttributes.HasThis, translatedGlobalType, Seq.empty))
                MethodAttributes.CompilerControlled
                ("__global_init@" + globalIndexString)

        Transpiler.includeMethodInput initialValueMethod glbl.Value transpilerInputBuilder

        match wasm.Exports.GetGlobalName(Wasm.Format.GlobalIdx index) with
        | true, globalExportName ->
            let globalTypeInstantiation = rtlib.InstantiatedGlobal glbl.Type.Type

            let field =
                DefinitionHelpers.addFieldDefinition
                    moduleClassDefinition
                    globalTypeInstantiation.FieldSignature
                    FieldAttributes.InitOnly
                    globalFieldName

            DefinitionHelpers.addInstanceFieldGetter
                moduleClassDefinition
                globalTypeInstantiation.Instantiation
                PropertyAttributes.None
                (MethodAttributes.Public ||| MethodAttributes.HideBySig)
                field
                (mangleMemberName globalExportName)

            members.Globals[index] <-
                GlobalMember.DefinedExport(field, globalTypeInstantiation.ValueAccessor, globalTypeInstantiation.SetValueHelper)
            
            let il = moduleInstanceConstructor.Instructions
            il.Add(CilInstruction CilOpCodes.Ldarg_0)
            il.Add(CilInstruction CilOpCodes.Dup)
            il.Add(CilInstruction(CilOpCodes.Call, initialValueMethod))
            il.Add(CilInstruction(if isMutableGlobal then CilOpCodes.Ldc_I4_1 else CilOpCodes.Ldc_I4_0))
            il.Add(CilInstruction(CilOpCodes.Newobj, globalTypeInstantiation.Constructor))
            il.Add(CilInstruction(CilOpCodes.Stfld, field))
        | false, _ ->
            let field =
                DefinitionHelpers.addFieldDefinition
                    moduleClassDefinition
                    (FieldSignature translatedGlobalType)
                    (if isMutableGlobal then FieldAttributes.PrivateScope else FieldAttributes.InitOnly)
                    globalFieldName

            let setter =
                if isMutableGlobal then
                    let signature = MethodSignature(
                        CallingConventionAttributes.Default,
                        moduleClassDefinition.Module.CorLibTypeFactory.Void,
                        [| translatedGlobalType; mainClassSignature |]
                    )

                    let definition =
                        DefinitionHelpers.addMethodDefinition
                            moduleClassDefinition
                            signature
                            MethodAttributes.Static
                            ("__global_set@" + globalIndexString)

                    definition.CilMethodBody <- CilMethodBody definition
                    let il = definition.CilMethodBody.Instructions
                    il.Add(CilInstruction CilOpCodes.Ldarg_1)
                    il.Add(CilInstruction CilOpCodes.Ldarg_0)
                    il.Add(CilInstruction(CilOpCodes.Stfld, field))
                    il.Add(CilInstruction CilOpCodes.Ret)

                    ValueSome definition
                else
                    ValueNone

            members.Globals[index] <- GlobalMember.Defined(field, setter)

            let il = moduleInstanceConstructor.Instructions
            il.Add(CilInstruction CilOpCodes.Ldarg_0)
            il.Add(CilInstruction CilOpCodes.Dup)
            il.Add(CilInstruction(CilOpCodes.Call, initialValueMethod))
            il.Add(CilInstruction CilOpCodes.Stfld)
