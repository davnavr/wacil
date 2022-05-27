module Wacil.Compiler.Emit.Module

open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

open Wacil.Compiler.Helpers
open Wacil.Compiler.Wasm

// TODO: Should the option to generate a module instead of an assembly be allowed? Why would someone want a module?

let inline toEntityHandle handle = (^H : (static member op_Implicit: 'H -> EntityHandle) (handle))

let generateCoreLibraryReference (builder: MetadataBuilder) =
    builder.AddAssemblyReference (
        builder.GetOrAddString "System.Runtime",
        System.Version(6, 0, 0, 0), // TODO: Is this the right version?
        StringHandle(), // TODO: Is culture value allowed to be empty?
        failwith "pkeyh",
        Unchecked.defaultof<AssemblyFlags>,
        BlobHandle()
    )

type CoreLibraryTypes = { Object: TypeReferenceHandle }

let generateCoreLibraryTypes (coreLibraryReference: AssemblyReferenceHandle) (builder: MetadataBuilder) =
    let coreLibraryHandle = toEntityHandle coreLibraryReference
    let systemNamespaceHandle = builder.GetOrAddString "System"

    let addSystemType name = builder.AddTypeReference(coreLibraryHandle, systemNamespaceHandle, builder.GetOrAddString name)

    // TODO: I thought TypeSpec has special syntax for primitive types (int32, int64, etc.)
    { CoreLibraryTypes.Object = addSystemType "Object" }

let generateMainClass (options: Options) (builder: MetadataBuilder) =
    let namespaceStringHandle =
        if System.String.IsNullOrEmpty options.Namespace
        then StringHandle()
        else builder.GetOrAddString(options.Namespace)

    builder.AddTypeDefinition (
        TypeAttributes.Sealed ||| TypeAttributes.Public,
        namespaceStringHandle,
        builder.GetOrAddString(options.Name),
        failwith "System.Object",
        failwith "fields",
        failwith "methods"
    )

let compileToBlobBuilder (options: Options) (webAssemblyModule: Format.Module) (builder: BlobBuilder) =
    let metadata = MetadataBuilder()
    let coreLibraryReference = generateCoreLibraryReference metadata
    let coreLibraryTypes = generateCoreLibraryTypes coreLibraryReference metadata

    raise (System.NotImplementedException())
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
