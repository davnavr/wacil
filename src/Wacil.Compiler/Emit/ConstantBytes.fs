/// Helper module for defining constant byte arrays.
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.ConstantBytes

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types

open AsmResolver.PE.DotNet.Metadata.Tables.Rows;

let createFieldFactory (implDetailsClass: TypeDefinition) (syslib: SystemLibrary.References) =
    let sizedBytesLookup =
        let lookup = System.Collections.Generic.Dictionary<int, FieldSignature>()
        fun size ->
            if size <= 0 then invalidArg (nameof size) "size of constant byte array must be greater than zero"

            match lookup.TryGetValue size with
            | true, existing -> existing
            | false, _ ->
                let bytesTypeDefinition =
                    TypeDefinition(
                        System.String.Empty,
                        "b" + string size,
                        TypeAttributes.NestedAssembly ||| TypeAttributes.ExplicitLayout ||| TypeAttributes.Sealed
                    )

                bytesTypeDefinition.ClassLayout <- ClassLayout(1us, uint32 size)
                bytesTypeDefinition.BaseType <- syslib.ValueType
                implDetailsClass.NestedTypes.Add bytesTypeDefinition

                let bytesFieldSignature = FieldSignature(TypeDefOrRefSignature bytesTypeDefinition)
                lookup[size] <- bytesFieldSignature
                bytesFieldSignature

    fun (bytes: byte[]) name ->
        // A private field containing the actual bytes
        let dataContentsField =
            FieldDefinition(
                name,
                FieldAttributes.Assembly ||| FieldAttributes.Static ||| FieldAttributes.InitOnly ||| FieldAttributes.HasFieldRva,
                sizedBytesLookup bytes.Length
            )

        implDetailsClass.Fields.Add dataContentsField
        dataContentsField.FieldRva <- AsmResolver.DataSegment bytes

        dataContentsField
