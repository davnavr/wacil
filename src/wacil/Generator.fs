module wacil.Generator

open System
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable

open Wasm.Format
open Wasm.Format.Types

[<NoComparison; NoEquality>]
type FileType = | Assembly | Netmodule

[<RequireQualifiedAccess>]
module FileType =
    let extension fileType =
        match fileType with
        | Assembly _ -> "dll"
        | Netmodule -> "netmodule"

type Options =
    { ModuleFileName: string
      FileType: FileType
      HighEntropyVA: bool
      Namespace: string
      MainClassName: string }

[<RequireQualifiedAccess>]
module Generate =
    let addModuleRow options (metadata: MetadataBuilder) =
        let mvid = Guid.NewGuid() // TODO: Figure out how to deterministically generate MVID

        metadata.AddModule (
            0,
            metadata.GetOrAddString(options.ModuleFileName + "." + FileType.extension options.FileType),
            metadata.GetOrAddGuid mvid,
            GuidHandle(),
            GuidHandle()
        )
        |> ignore

    let addCoreAssembly options (metadata: MetadataBuilder) =
        // TODO: Figure out how to allow usage of other core assemblies?
        metadata.AddAssemblyReference (
            metadata.GetOrAddString "System.Runtime",
            Version(5, 0, 0, 0),
            StringHandle(),
            metadata.GetOrAddBlob [| 0xb0uy; 0x3fuy; 0x5fuy; 0x7fuy; 0x11uy; 0xd5uy; 0x0auy; 0x3auy |],
            Unchecked.defaultof<_>,
            BlobHandle()
        )

    let generateAssemblyRow options (metadata: MetadataBuilder) =
        match options.FileType with
        | Assembly ->
            metadata.AddAssembly (
                metadata.GetOrAddString options.ModuleFileName,
                Version(1, 0, 0, 0), // TODO: Have option to set version of assembly.
                StringHandle(),
                BlobHandle(),
                AssemblyFlags.EnableJitCompileTracking,
                AssemblyHashAlgorithm.Sha1
            )
            |> ValueSome
        | Netmodule -> ValueNone

    let generateValType (t: ValType) (signature: BlobBuilder) =
        match t with
        | ValType.NumType nt ->
            match nt with
            | I32 -> signature.WriteByte 0x8uy // I4
            | I64 -> signature.WriteByte 0xAuy // I8
            | F32 -> signature.WriteByte 0xCuy // R4
            | F64 -> signature.WriteByte 0xDuy // R8
        | ValType.RefType rt -> failwithf "TODO: reference types not supported %A" rt

    let generateFunctionSignature (types: TypeSection) { Function.Type = t } (metadata: MetadataBuilder) =
        let signature = &types.Item t
        let signature' = BlobBuilder()
        signature'.WriteByte 0uy // Default
        signature'.WriteCompressedInteger signature.Parameters.Length // ParamCount

        match signature.Results.Length with
        | 0 -> signature'.WriteByte 1uy // VOID
        | 1 -> generateValType signature.Results.[0] signature'
        | _ -> failwithf "TODO: Add support for multiple return types (use tuple types?) %A" signature.Results

        for i = 0 to signature.Parameters.Length - 1 do
            generateValType signature.Results.[i] signature'

        metadata.GetOrAddBlob signature'

    let toBlob (ValidatedModule file) options (destination: BlobBuilder) =
        let sections = getKnownSections file

        let metadata = MetadataBuilder()
        let bodies = BlobBuilder()

        do addModuleRow options metadata
        let mscorlib = addCoreAssembly options metadata
        let assembly = generateAssemblyRow options metadata

        let addCoreType =
            let system = metadata.GetOrAddString "System"
            fun name ->
                metadata.AddTypeReference(AssemblyReferenceHandle.op_Implicit mscorlib, system, metadata.GetOrAddString name)

        let object = addCoreType "Object"

        metadata.SetCapacity(TableIndex.TypeDef, rowCount = 2)

        let globals =
            metadata.AddTypeDefinition (
                Unchecked.defaultof<_>,
                StringHandle(),
                metadata.GetOrAddString "<Module>",
                TypeReferenceHandle.op_Implicit object,
                FieldDefinitionHandle(),
                MethodDefinitionHandle()
            )

        let functions =
            let mutable funci = MethodDefinitionHandle()

            match sections with
            | { TypeSection = ValueSome types; FunctionSection = ValueSome funcs; CodeSection = ValueSome code } ->
                for i = 0 to funcs.Length - 1 do
                    let func = funcs.[funcs.First + uint32 i]
                    let func' =
                        metadata.AddMethodDefinition (
                            MethodAttributes.Static, // TODO: If function is exported, make it public
                            MethodImplAttributes.IL,
                            metadata.GetOrAddString("func#" + string i), // TODO: If function is exported, use its name
                            (generateFunctionSignature types func metadata),
                            failwith "TODO: Get method body",
                            ParameterHandle()
                        )

                    if i = 0 then funci <- func'
            | _ -> ()

            funci

        let gclass =
            metadata.AddTypeDefinition (
                TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Abstract,
                (if String.IsNullOrEmpty options.Namespace then StringHandle() else metadata.GetOrAddString options.Namespace),
                metadata.GetOrAddString options.MainClassName,
                TypeReferenceHandle.op_Implicit object,
                FieldDefinitionHandle(),
                functions
            )

        let cliMetadataRoot = MetadataRootBuilder metadata
        let pe =
            let mutable peDllCharacteristics =
                DllCharacteristics.NoSeh
                ||| DllCharacteristics.DynamicBase
                ||| DllCharacteristics.NxCompatible

            if options.HighEntropyVA then
                peDllCharacteristics <- peDllCharacteristics ||| DllCharacteristics.HighEntropyVirtualAddressSpace

            let mutable peImageCharacteristics = Characteristics.ExecutableImage

            ManagedPEBuilder (
                header = PEHeaderBuilder (
                    dllCharacteristics = peDllCharacteristics,
                    imageCharacteristics = Characteristics.ExecutableImage
                ),
                metadataRootBuilder = cliMetadataRoot,
                ilStream = bodies
            )

        pe.Serialize destination |> ignore

    let toStream file options (destination: System.IO.Stream) =
        if not destination.CanWrite then
            invalidArg (nameof destination) "The destination stream where the CIL file is written to must support writing"

        let peblob = BlobBuilder()
        toBlob file options peblob
        peblob.WriteContentTo destination
