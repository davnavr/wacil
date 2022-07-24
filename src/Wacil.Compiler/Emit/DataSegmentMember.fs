[<RequireQualifiedAccess;>]
module internal Wacil.Compiler.Emit.DataSegmentMember

open System.Collections.Immutable

open Wacil.Compiler
open Wacil.Compiler.Wasm.Validation.Table

open AsmResolver.PE.DotNet.Metadata.Tables.Rows
open AsmResolver.PE.DotNet.Cil;

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types
open AsmResolver.DotNet.Code.Cil

let translateDataSegments
    (constantByteFactory: byte[] -> string -> FieldDefinition)
    (syslib: SystemLibrary.References)
    (rtlib: RuntimeLibrary.References)
    (moduleClassDefinition: TypeDefinition)
    (wasmDataSegments: ImmutableArray<ValidData>)
    (members: ModuleMembers)
    (transpilerInputBuilder: ResizeArray<Transpiler.Input>)
    (moduleInstanceConstructor: CilMethodBody)
    (moduleStaticInitializer: CilMethodBody)
    =
    let tyByte = moduleClassDefinition.Module.CorLibTypeFactory.Byte
    let tyByteSignature = TypeSpecification tyByte :> ITypeDefOrRef
    let dataBytesSignature = FieldSignature(tyByte.MakeSzArrayType())
    let activeDataOffsetSignature =
        MethodSignature(CallingConventionAttributes.HasThis, moduleClassDefinition.Module.CorLibTypeFactory.Int32, Seq.empty)

    for i in 0..wasmDataSegments.Length - 1 do
        let data = wasmDataSegments[i]
        let name = "__data_segment@" + string i

        let actualBytesField = constantByteFactory (data.Bytes.AsSpan().ToArray()) name

        match data.Mode with
        | ValueNone ->
            // Translating an passive data segment, raw bytes need to be copied to an additional static field
            let dataBytesField =
                DefinitionHelpers.addFieldDefinition
                    moduleClassDefinition
                    dataBytesSignature
                    (FieldAttributes.Static ||| FieldAttributes.InitOnly)
                    name

            members.DataSegments[i] <- DataSegmentMember.Passive dataBytesField

            let il = moduleStaticInitializer.Instructions
            CilHelpers.emitArrayFromModuleData syslib data.Bytes.Length tyByteSignature actualBytesField il
            il.Add(CilInstruction(CilOpCodes.Stsfld, dataBytesField))
        | ValueSome(activeDataSegment) ->
            // Translating an active data segment does not require generation of an additional static field

            let dataOffsetFunction =
                DefinitionHelpers.addMethodDefinition
                    moduleClassDefinition
                    activeDataOffsetSignature
                    MethodAttributes.CompilerControlled
                    (name + "_offset")

            members.DataSegments[i] <- DataSegmentMember.Active

            dataOffsetFunction.CilMethodBody <- CilMethodBody dataOffsetFunction

            transpilerInputBuilder.Add
                { Transpiler.Expression = activeDataSegment.Offset
                  Transpiler.Body = dataOffsetFunction.CilMethodBody }

            let il = moduleInstanceConstructor.Instructions
            il.Add(CilInstruction CilOpCodes.Ldarg_0)
            match members.Memories[int32 activeDataSegment.Memory] with
            | MemoryMember.Defined(memory) ->
                il.Add(CilInstruction(CilOpCodes.Ldfld, memory))
            | MemoryMember.Imported(import, memory) ->
                il.Add(CilInstruction(CilOpCodes.Ldfld, import))
                il.Add(CilInstruction(CilOpCodes.Ldfld, memory))

            il.Add(CilInstruction CilOpCodes.Ldarg_0)
            il.Add(CilInstruction(CilOpCodes.Call, dataOffsetFunction))
            
            CilHelpers.emitArrayFromModuleData syslib data.Bytes.Length tyByteSignature actualBytesField il
            il.Add(CilInstruction(CilOpCodes.Call, rtlib.Memory.WriteArray))
