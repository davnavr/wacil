/// Helper module for translation of WebAssembly module memories.
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.MemoryTranslator

open Wacil.Compiler
open Wacil.Compiler.Helpers.Collections

open AsmResolver.PE.DotNet.Metadata.Tables.Rows
open AsmResolver.PE.DotNet.Cil;

open AsmResolver.DotNet
open AsmResolver.DotNet.Code.Cil

let translateModuleMemories
    mangleMemberName
    (rtlib: RuntimeLibrary.References)
    (moduleClassDefinition: TypeDefinition)
    (wasm: Wasm.Validation.ValidModule)
    (memoryDefinitionImplementation: MemoryImplementation)
    (members: ModuleMembers)
    (moduleInstanceConstructor: CilMethodBody)
    =
    let firstDefinedIndex = wasm.Imports.Imports.Memories.Length
    let memoryDefinitionInstantiation = rtlib.InstantiatedMemory memoryDefinitionImplementation

    for i in 0..wasm.Memories.Length - 1 do
        let memory = wasm.Memories[i]
        let index = firstDefinedIndex + i

        let field =
            DefinitionHelpers.addFieldDefinition
                moduleClassDefinition
                memoryDefinitionInstantiation.FieldSignature
                FieldAttributes.InitOnly
                ("__memory@" + string index)

        match wasm.Exports.GetMemoryName(Wasm.Format.MemIdx index) with
        | true, memoryExportName ->
            DefinitionHelpers.addInstanceFieldGetter
                moduleClassDefinition
                memoryDefinitionInstantiation.Signature
                PropertyAttributes.None
                (MethodAttributes.Public ||| MethodAttributes.HideBySig)
                field
                (mangleMemberName memoryExportName)
        | false, _ -> ()

        let maximum =
            match memory.Maximum with
            | ValueSome m -> int32 m
            | ValueNone -> -1

        members.Memories[index] <- MemoryMember.Defined(field, memoryDefinitionInstantiation)

        match memoryDefinitionInstantiation.Constructor with
        | Some ctor ->
            // Append memory initialization code to the module constructor
            let il = moduleInstanceConstructor.Instructions
            il.Add(CilInstruction CilOpCodes.Ldarg_0)
            il.Add(CilInstruction.CreateLdcI4(int32 memory.Minimum))
            il.Add(CilInstruction.CreateLdcI4 maximum)
            il.Add(CilInstruction(CilOpCodes.Newobj, ctor))
            il.Add(CilInstruction(CilOpCodes.Stfld, field))
        | None ->
            failwith "TODO: Prevent IMemory32 from being used for defined memories"
