[<RequireQualifiedAccess;>]
module internal Wacil.Compiler.Emit.ElementSegmentMember

open System.Collections.Immutable

open Wacil.Compiler
open Wacil.Compiler.Wasm.Validation.Table

open AsmResolver.PE.DotNet.Metadata.Tables.Rows
open AsmResolver.PE.DotNet.Cil;

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types
open AsmResolver.DotNet.Code.Cil

let translateElementSegments
    (translateValType: _ -> TypeSignature)
    (rtlib: RuntimeLibrary.References)
    (moduleClassDefinition: TypeDefinition)
    (wasmElementSegments: ImmutableArray<ValidElement>)
    (members: ModuleMembers)
    (transpilerInputBuilder: ResizeArray<Transpiler.Input>)
    (moduleInstanceConstructor: CilMethodBody)
    =
    let segmentOffsetSignature = MethodSignature.CreateInstance moduleClassDefinition.Module.CorLibTypeFactory.Int32

    for elementSegmentIndex in 0..wasmElementSegments.Length - 1 do
        let element = wasmElementSegments[elementSegmentIndex]

        match element.Mode with
        | ValidElementMode.Declarative -> ()
        | ValidElementMode.Passive | ValidElementMode.Active _ ->
            let name = "element_segment@" + string elementSegmentIndex
            let tableInstantiationType = rtlib.InstantiatedTable element.Type
            let translatedElementType = translateValType(Wasm.Format.ValType.ofRefType element.Type)
            let elementArrayType = translatedElementType.MakeSzArrayType()

            let generateElementsHelper =
                let definition =
                    DefinitionHelpers.addMethodDefinition
                        moduleClassDefinition
                        (MethodSignature.CreateInstance elementArrayType)
                        MethodAttributes.CompilerControlled
                        (name + "_elements")

                definition.CilMethodBody <- CilMethodBody definition
                let il = definition.CilMethodBody.Instructions
                il.Add(CilInstruction.CreateLdcI4 element.Elements.Length)
                il.Add(CilInstruction(CilOpCodes.Newarr, TypeSpecification translatedElementType))

                for elementExpressionIndex in 0..element.Elements.Length - 1 do
                    let initializer =
                        DefinitionHelpers.addMethodDefinition
                            moduleClassDefinition
                            (MethodSignature.CreateInstance translatedElementType)
                            MethodAttributes.CompilerControlled
                            (name + "-" + string elementExpressionIndex)

                    initializer.CilMethodBody <- CilMethodBody initializer
                    transpilerInputBuilder.Add
                        { Transpiler.Body = initializer.CilMethodBody
                          Transpiler.Expression = element.Elements[elementExpressionIndex] }

                    il.Add(CilInstruction CilOpCodes.Dup) // Push the array of elements onto the stack
                    il.Add(CilInstruction.CreateLdcI4 elementExpressionIndex)
                    il.Add(CilInstruction CilOpCodes.Ldarg_0)
                    il.Add(CilInstruction(CilOpCodes.Call, initializer))
                    il.Add(CilInstruction CilOpCodes.Stelem_Ref)

                il.Add(CilInstruction CilOpCodes.Ret)

                definition

            match element.Mode with
            | ValidElementMode.Passive ->
                let elementSegmentField =
                    DefinitionHelpers.addFieldDefinition
                        moduleClassDefinition
                        (FieldSignature elementArrayType)
                        FieldAttributes.PrivateScope
                        name

                let elementSegmentGetter =
                    let definition =
                        DefinitionHelpers.addMethodDefinition
                            moduleClassDefinition
                            (MethodSignature.CreateInstance elementArrayType)
                            MethodAttributes.CompilerControlled
                            name

                    definition.CilMethodBody <- CilMethodBody definition
                    let il = definition.CilMethodBody.Instructions
                    let returnElementSegment = CilInstruction CilOpCodes.Ldarg_0
                    il.Add(CilInstruction CilOpCodes.Ldarg_0)
                    il.Add(CilInstruction(CilOpCodes.Ldfld, elementSegmentField))
                    il.Add(CilInstruction(CilOpCodes.Brtrue_S, CilInstructionLabel returnElementSegment))
                    
                    il.Add(CilInstruction CilOpCodes.Ldarg_0)
                    il.Add(CilInstruction CilOpCodes.Dup)
                    il.Add(CilInstruction(CilOpCodes.Call, generateElementsHelper))
                    il.Add(CilInstruction(CilOpCodes.Stfld, elementSegmentField))

                    il.Add returnElementSegment
                    il.Add(CilInstruction(CilOpCodes.Ldfld, elementSegmentField))
                    il.Add(CilInstruction CilOpCodes.Ret)

                    definition

                let il = moduleInstanceConstructor.Instructions
                il.Add(CilInstruction CilOpCodes.Ldarg_0)
                il.Add(CilInstruction(CilOpCodes.Call, generateElementsHelper))
                il.Add(CilInstruction CilOpCodes.Pop)

                members.ElementSegments[elementSegmentIndex] <- ElementSegmentMember.Passive(elementSegmentField, elementSegmentGetter)
            | ValidElementMode.Active(table, offset) ->
                let segmentOffsetMethod =
                    DefinitionHelpers.addMethodDefinition
                        moduleClassDefinition
                        segmentOffsetSignature
                        MethodAttributes.CompilerControlled
                        (name + "_offset")

                segmentOffsetMethod.CilMethodBody <- CilMethodBody segmentOffsetMethod
                transpilerInputBuilder.Add
                    { Transpiler.Body = segmentOffsetMethod.CilMethodBody; Transpiler.Expression = offset }

                let il = moduleInstanceConstructor.Instructions
                il.Add(CilInstruction.CreateLdcI4 element.Elements.Length)
                il.Add(CilInstruction CilOpCodes.Ldc_I4_0)
                il.Add(CilInstruction CilOpCodes.Ldarg_0)
                il.Add(CilInstruction(CilOpCodes.Call, segmentOffsetMethod))

                il.Add(CilInstruction CilOpCodes.Ldarg_0)
                match members.Tables[int32 table] with
                | TableMember.Defined(field, _) ->
                    il.Add(CilInstruction(CilOpCodes.Ldfld, field))
                | TableMember.Imported(import, field, _) ->
                    il.Add(CilInstruction(CilOpCodes.Ldfld, import))
                    il.Add(CilInstruction(CilOpCodes.Ldfld, field))

                il.Add(CilInstruction CilOpCodes.Ldarg_0)
                il.Add(CilInstruction(CilOpCodes.Call, generateElementsHelper))
                il.Add(CilInstruction(CilOpCodes.Call, tableInstantiationType.Initialize))
                
                members.ElementSegments[elementSegmentIndex] <- ElementSegmentMember.Active
            | ValidElementMode.Declarative -> raise(System.NotImplementedException())
