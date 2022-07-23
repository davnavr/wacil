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
    (moduleClassDefinition: TypeDefinition)
    (wasmDataSegments: ImmutableArray<ValidData>)
    (members: DataSegmentMember[])
    (transpilerInputBuilder: ResizeArray<Transpiler.Input>)
    (moduleInstanceConstructor: CilMethodBody)
    (moduleStaticInitializer: CilMethodBody)
    =
    let tyByte = moduleClassDefinition.Module.CorLibTypeFactory.Byte
    let dataBytesSignature = FieldSignature(tyByte.MakeSzArrayType())

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

            members[i] <- DataSegmentMember.Passive dataBytesField

            let il = moduleStaticInitializer.Instructions
            il.Add(CilInstruction.CreateLdcI4 data.Bytes.Length)
            il.Add(CilInstruction(CilOpCodes.Newarr, tyByte))
            il.Add(CilInstruction(CilOpCodes.Ldtoken, actualBytesField))
            il.Add(CilInstruction(CilOpCodes.Call, syslib.RuntimeHelpers.InitalizeArray))
            il.Add(CilInstruction(CilOpCodes.Stsfld, dataBytesField))
        | ValueSome(activeDataSegment) ->
            // Translating an active data segment does not require generation of an additional static field
            failwith "TODO: Add to instance ctor"
