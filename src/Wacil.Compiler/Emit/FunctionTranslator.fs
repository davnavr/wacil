/// Helper module for translating WebAssembly function definitions into .NET methods.
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.FunctionTranslator

open Wacil.Compiler
open Wacil.Compiler.Helpers.Collections

open AsmResolver.PE.DotNet.Metadata.Tables.Rows
open AsmResolver.PE.DotNet.Cil;

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types
open AsmResolver.DotNet.Code.Cil

let translateFunctionDefinitions
    mangleMemberName
    (markCompilerGenerated: CustomAttribute.Marker)
    (markCustomName: string -> CustomAttribute.Marker)
    (translateFuncType: _ -> MethodSignature)
    (wasmCustomNames: Wasm.CustomNames.Lookup option)
    (moduleClassDefinition: TypeDefinition)
    (moduleClassSignature: TypeDefOrRefSignature)
    (wasm: Wasm.Validation.ValidModule)
    (members: ModuleMembers)
    (transpilerInputBuilder: ResizeArray<Transpiler.Input>)
    =
    let firstDefinedIndex = wasm.Imports.Imports.Functions.Length
    let mutable parameterTypeBuilder = ArrayBuilder<TypeSignature>.Create()

    let markCustomFunctionName =
        match wasmCustomNames with
        | Some names ->
            fun index parent ->
                let name = names.GetFunctionName(Wasm.Format.FuncIdx index)
                if not(System.String.IsNullOrEmpty name) then
                    markCustomName name parent
        | None -> fun _ _ -> ()

    for i in 0..wasm.Functions.Length - 1 do
        let func = wasm.Functions[i]
        let index = firstDefinedIndex + i
        let functionIndexString = string index

        let name, accessibility =
            match wasm.Exports.GetFunctionName(Wasm.Format.FuncIdx index) with
            | true, functionExportName -> mangleMemberName functionExportName, MethodAttributes.Public
            | false, _ -> "__function@" + functionIndexString, MethodAttributes.CompilerControlled

        let translatedFunctionSignature = translateFuncType func.Type

        let translatedInstanceMethod =
            DefinitionHelpers.addMethodDefinition
                moduleClassDefinition
                translatedFunctionSignature
                (MethodAttributes.HideBySig ||| accessibility)
                name

        Transpiler.includeMethodInput translatedInstanceMethod func.Body transpilerInputBuilder

        markCustomFunctionName index translatedInstanceMethod

        let staticHelperMethod =
            let signature =
                parameterTypeBuilder.Clear()
                for ty in translatedFunctionSignature.ParameterTypes do parameterTypeBuilder.Add ty
                parameterTypeBuilder.Add moduleClassSignature
                MethodSignature(
                    CallingConventionAttributes.Default,
                    translatedFunctionSignature.ReturnType,
                    parameterTypeBuilder.ToArray()
                )

            let definition =
                DefinitionHelpers.addMethodDefinition
                    moduleClassDefinition
                    signature
                    MethodAttributes.Static
                    ("__call_function@" + functionIndexString)

            markCompilerGenerated definition
            definition.CilMethodBody <- CilMethodBody definition
            definition.ImplAttributes <- CilHelpers.methodImplAggressiveInlining
            let il = definition.CilMethodBody.Instructions
            let originalParameterCount = translatedFunctionSignature.ParameterTypes.Count
            il.Add(CilInstruction.CreateLdarg(Checked.uint16 originalParameterCount))
            for i = 0 to originalParameterCount - 1 do il.Add(CilInstruction.CreateLdarg(Checked.uint16 i))
            // TODO: Figure out if tail.call is valid even if byref parameters are used
            il.Add(CilInstruction CilOpCodes.Tailcall)
            il.Add(CilInstruction(CilOpCodes.Call, translatedInstanceMethod))
            il.Add(CilInstruction CilOpCodes.Ret)

            definition

        members.Functions[index] <- FunctionMember.Defined(translatedInstanceMethod, staticHelperMethod, func.Type)
