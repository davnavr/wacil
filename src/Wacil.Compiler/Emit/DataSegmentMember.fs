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
    (moduleClassDefinition: TypeDefinition)
    (wasmDataSegments: ImmutableArray<ValidData>)
    (members: DataSegmentMember[])
    (moduleInstanceConstructor: CilMethodBody)
    (moduleStaticInitializer: CilMethodBody)
    =
    let dataBytesSignature = FieldSignature(moduleClassDefinition.Module.CorLibTypeFactory.Byte.MakeSzArrayType())

    for i in 0..wasmDataSegments.Length - 1 do
        let data = wasmDataSegments[i]
        let dataIndexString = string i

        let dataBytesField =
            DefinitionHelpers.addFieldDefinition
                moduleClassDefinition
                dataBytesSignature
                (FieldAttributes.Static ||| FieldAttributes.InitOnly)
                ("__data_segment@" + dataIndexString)

        ()
