[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.TableMember

open Wacil.Compiler
open Wacil.Compiler.Helpers.Collections

open AsmResolver.PE.DotNet.Metadata.Tables.Rows
open AsmResolver.PE.DotNet.Cil;

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types
open AsmResolver.DotNet.Code.Cil

let translateModuleTables
    mangleMemberName
    (rtlib: RuntimeLibrary.References)
    (moduleClassDefinition: TypeDefinition)
    (wasm: Wasm.Validation.ValidModule)
    (members: ModuleMembers)
    (moduleInstanceConstructor: CilMethodBody)
    =
    let firstDefinedIndex = wasm.Imports.Imports.Tables.Length

    for i in 0..wasm.Tables.Length - 1 do
        let table = wasm.Tables[i]
        let index = firstDefinedIndex + i

        let instantiatedTableType = rtlib.InstantiatedTable table.ElementType

        let field =
            DefinitionHelpers.addFieldDefinition
                moduleClassDefinition
                instantiatedTableType.FieldSignature
                FieldAttributes.InitOnly
                ("__table@" + string index)

        match wasm.Exports.GetTableName(Wasm.Format.TableIdx index) with
        | true, memoryExportName ->
            DefinitionHelpers.addInstanceFieldGetter
                moduleClassDefinition
                instantiatedTableType.Instantiation
                PropertyAttributes.None
                (MethodAttributes.Public ||| MethodAttributes.HideBySig)
                field
                (mangleMemberName memoryExportName)
        | false, _ -> ()

        members.Tables[index] <- TableMember.Defined field

        let maximum =
            match table.Limits.Maximum with
            | ValueSome m -> int32 m
            | ValueNone -> -1

        // Append table initialization code to the module constructor
        let il = moduleInstanceConstructor.Instructions
        il.Add(CilInstruction CilOpCodes.Ldarg_0)
        il.Add(CilInstruction.CreateLdcI4(int32 table.Limits.Minimum))
        il.Add(CilInstruction.CreateLdcI4 maximum)
        il.Add(CilInstruction(CilOpCodes.Newobj, instantiatedTableType.Constructor))
        il.Add(CilInstruction(CilOpCodes.Stfld, field))
