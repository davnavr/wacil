module wacil.Generator

open System
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable

open Wasm.Format

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
      HighEntropyVA: bool }

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

    let toBlob (ValidatedModule file) options (destination: BlobBuilder) =
        let metadata = MetadataBuilder()
        let bodies = BlobBuilder()

        do addModuleRow options metadata
        let mscorlib = addCoreAssembly options metadata
        let assembly = generateAssemblyRow options metadata

        metadata.SetCapacity(TableIndex.TypeDef, rowCount = 2)

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
