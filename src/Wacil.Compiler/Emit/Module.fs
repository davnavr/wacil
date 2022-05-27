module Wacil.Compiler.Emit.Module

open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

open Wacil.Compiler.Helpers
open Wacil.Compiler.Wasm

// TODO: Should the option to generate a module instead of an assembly be allowed? Why would someone want a module?

let inline toEntityHandle handle = (^H : (static member op_Implicit: 'H -> EntityHandle) (handle))

let coreLibraryPublicKeyToken = [| 0xb0uy; 0x3fuy; 0x5fuy; 0x7fuy; 0x11uy; 0xd5uy; 0x0auy; 0x3auy |]

let generateCoreLibraryReference (builder: MetadataBuilder) =
    let publicKeyToken = BlobBuilder(8)
    publicKeyToken.WriteBytes coreLibraryPublicKeyToken

    builder.AddAssemblyReference (
        builder.GetOrAddString "System.Runtime",
        System.Version(6, 0, 0, 0),
        StringHandle(),
        builder.GetOrAddBlob(publicKeyToken),
        Unchecked.defaultof<AssemblyFlags>,
        BlobHandle()
    )

type CoreLibraryTypes = { Object: TypeReferenceHandle }

let generateCoreLibraryTypes (coreLibraryReference: AssemblyReferenceHandle) (builder: MetadataBuilder) =
    let coreLibraryHandle = toEntityHandle coreLibraryReference
    let systemNamespaceHandle = builder.GetOrAddString "System"

    let addSystemType name = builder.AddTypeReference(coreLibraryHandle, systemNamespaceHandle, builder.GetOrAddString name)

    { CoreLibraryTypes.Object = addSystemType "Object" }

let generateMainClass (options: Options) coreLibraryTypes (builder: MetadataBuilder) =
    let namespaceStringHandle =
        if System.String.IsNullOrEmpty options.Namespace
        then StringHandle()
        else builder.GetOrAddString(options.Namespace)

    builder.AddTypeDefinition (
        TypeAttributes.Sealed ||| TypeAttributes.Public,
        namespaceStringHandle,
        builder.GetOrAddString(options.Name),
        toEntityHandle coreLibraryTypes.Object,
        FieldDefinitionHandle(), // TODO: Should these be set to something?
        MethodDefinitionHandle()
    )

let compileToBlobBuilder (options: Options) (webAssemblyModule: Format.Module) (builder: BlobBuilder) =
    let metadata = MetadataBuilder()
    let coreLibraryReference = generateCoreLibraryReference metadata
    let coreLibraryTypes = generateCoreLibraryTypes coreLibraryReference metadata

    metadata.AddTypeDefinition (
        TypeAttributes.NotPublic,
        StringHandle(),
        metadata.GetOrAddString("<Module>"),
        toEntityHandle coreLibraryTypes.Object,
        FieldDefinitionHandle(),
        MethodDefinitionHandle()
    )
    |> ignore

    let mainTypeDefinition = generateMainClass options coreLibraryTypes metadata
    
    ()

let compileToStream options webAssemblyModule (stream: System.IO.Stream) =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanWrite then invalidArg (nameof stream) "Destination stream must support writing"
        let builder = BlobBuilder()
        compileToBlobBuilder options webAssemblyModule builder
        builder.WriteContentTo(stream)
    finally
        stream.Close()
