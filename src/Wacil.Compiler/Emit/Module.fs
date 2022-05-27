module Wacil.Compiler.Emit.Module

open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable

open Wacil.Compiler.Wasm

// TODO: Should the option to generate a module instead of an assembly be allowed? Why would someone want a module?

let inline toEntityHandle handle = (^H : (static member op_Implicit: 'H -> EntityHandle) (handle))

let coreLibraryPublicKeyToken = [| 0xb0uy; 0x3fuy; 0x5fuy; 0x7fuy; 0x11uy; 0xd5uy; 0x0auy; 0x3auy |]

let generateCoreLibraryReference (builder: MetadataBuilder) =
    let publicKeyToken = BlobBuilder(8)
    publicKeyToken.WriteBytes coreLibraryPublicKeyToken

    builder.AddAssemblyReference(
        builder.GetOrAddString "System.Runtime",
        System.Version(6, 0, 0, 0),
        StringHandle(),
        builder.GetOrAddBlob(publicKeyToken),
        Unchecked.defaultof<AssemblyFlags>,
        BlobHandle()
    )

type CoreLibraryTypes =
    { Object: TypeReferenceHandle
      TargetFrameworkAttribute: TypeReferenceHandle }

let generateCoreLibraryTypes (coreLibraryReference: AssemblyReferenceHandle) (builder: MetadataBuilder) =
    let coreLibraryHandle = toEntityHandle coreLibraryReference
    let systemNamespaceHandle = builder.GetOrAddString "System"

    let addSystemType name = builder.AddTypeReference(coreLibraryHandle, systemNamespaceHandle, builder.GetOrAddString name)

    { CoreLibraryTypes.Object = addSystemType "Object"
      TargetFrameworkAttribute = builder.AddTypeReference(
            coreLibraryHandle,
            builder.GetOrAddString "System.Runtime.Versioning",
            builder.GetOrAddString "TargetFrameworkAttribute"
        ) }

let generateMainClass
    (options: Options)
    (coreLibraryTypes: CoreLibraryTypes)
    (methodBodyBuilder: MethodBodyStreamEncoder)
    (builder: MetadataBuilder)
    =
    let constructorMethodName = builder.GetOrAddString ".ctor"

    let mainConstructorSignature =
        let blob = BlobEncoder(BlobBuilder(4))
        blob.MethodSignature(isInstanceMethod = true).Parameters(
            0, 
            (fun (encoder: ReturnTypeEncoder) -> encoder.Void()),
            ignore
        )
        builder.GetOrAddBlob(blob.Builder)

    let mainConstructorBody =
        let systemObjectConstructor = builder.AddMemberReference(
            toEntityHandle coreLibraryTypes.Object,
            constructorMethodName,
            mainConstructorSignature
        )

        let body = InstructionEncoder(BlobBuilder())
        body.OpCode(ILOpCode.Ldarg_0)
        body.Call(systemObjectConstructor)
        body.OpCode(ILOpCode.Ret)
        methodBodyBuilder.AddMethodBody(body, 1, StandaloneSignatureHandle(), MethodBodyAttributes.InitLocals)

    let constructor = builder.AddMethodDefinition(
        MethodAttributes.Public ||| MethodAttributes.RTSpecialName ||| MethodAttributes.SpecialName,
        MethodImplAttributes.IL,
        constructorMethodName,
        mainConstructorSignature,
        mainConstructorBody,
        MetadataTokens.ParameterHandle(1)
    )

    let namespaceStringHandle =
        if System.String.IsNullOrEmpty options.Namespace
        then StringHandle()
        else builder.GetOrAddString(options.Namespace)

    builder.AddTypeDefinition (
        TypeAttributes.Sealed ||| TypeAttributes.Public,
        namespaceStringHandle,
        builder.GetOrAddString(options.Name),
        toEntityHandle coreLibraryTypes.Object,
        MetadataTokens.FieldDefinitionHandle(1),
        constructor
    )

let generateTargetFrameworkAttribute assembly (coreLibraryTypes: CoreLibraryTypes) (builder: MetadataBuilder) =
    let attributeConstructorSignature =
        let blob = BlobEncoder(BlobBuilder(4))
        blob.MethodSignature(isInstanceMethod = true).Parameters(
            1,
            (fun (encoder: ReturnTypeEncoder) -> encoder.Void()),
            fun (encoder: ParametersEncoder) -> encoder.AddParameter().Type().String() |> ignore
        )
        builder.GetOrAddBlob(blob.Builder)

    let targetFrameworkConstructor = builder.AddMemberReference(
        toEntityHandle coreLibraryTypes.TargetFrameworkAttribute,
        builder.GetOrAddString(".ctor"),
        attributeConstructorSignature
    )

    let value =
        let blob = BlobEncoder(BlobBuilder(64))
        blob.CustomAttributeSignature(
            (fun (encoder: FixedArgumentsEncoder) -> encoder.AddArgument().Scalar().Constant(box ".NETCoreApp,Version=v6.0")),
            (fun (encoder: CustomAttributeNamedArgumentsEncoder) -> encoder.Count(1).AddArgument(
                true,
                (fun (encoder: NamedArgumentTypeEncoder) -> encoder.ScalarType().String()),
                (fun (encoder: NameEncoder) -> encoder.Name("FrameworkDisplayName")),
                fun (encoder: LiteralEncoder) -> encoder.Scalar().Constant(System.String.Empty)
            ))
        )
        builder.GetOrAddBlob(blob.Builder)

    builder.AddCustomAttribute(
        toEntityHandle(assembly: AssemblyDefinitionHandle),
        targetFrameworkConstructor,
        value
    )
    |> ignore

let deterministicIdProvider (content: seq<Blob>): BlobContentId =
    // let builder = BlobBuilder()
    // for blob in content do
    //     for b in blob.GetBytes() do printfn "0x%02X" b
    // Unchecked.defaultof<_>
    BlobContentId(System.Guid.Empty, 0u)

let compileToBlobBuilder (options: Options) (webAssemblyModule: Format.Module) (builder: BlobBuilder) =
    let metadata = MetadataBuilder()
    let methodBodyBuilder = MethodBodyStreamEncoder(BlobBuilder())

    let coreLibraryReference = generateCoreLibraryReference metadata
    let coreLibraryTypes = generateCoreLibraryTypes coreLibraryReference metadata

    let outputModuleName = metadata.GetOrAddString(options.Name)

    metadata.AddModule(
        0,
        outputModuleName,
        metadata.GetOrAddGuid(System.Guid.NewGuid()), // TODO: Generate a GUID deterministically.
        GuidHandle(),
        GuidHandle()
    )
    |> ignore

    let assembly = metadata.AddAssembly(
        outputModuleName,
        System.Version(0, 0, 0, 0),
        StringHandle(),
        BlobHandle(),
        Unchecked.defaultof<AssemblyFlags>,
        AssemblyHashAlgorithm.Sha1
    )

    generateTargetFrameworkAttribute assembly coreLibraryTypes metadata

    metadata.AddTypeDefinition(
        TypeAttributes.NotPublic,
        StringHandle(),
        metadata.GetOrAddString("<Module>"),
        toEntityHandle coreLibraryTypes.Object,
        MetadataTokens.FieldDefinitionHandle(1),
        MetadataTokens.MethodDefinitionHandle(1)
    )
    |> ignore

    let mainTypeDefinition = generateMainClass options coreLibraryTypes methodBodyBuilder metadata
    
    let metadataRootBuilder = MetadataRootBuilder(metadata)

    let portableExecutableBuilder = new ManagedPEBuilder(
        new PEHeaderBuilder (
            machine = Machine.I386,
            imageCharacteristics = (Characteristics.ExecutableImage ||| Characteristics.Dll |||
                Characteristics.LineNumsStripped ||| Characteristics.LocalSymsStripped ||| Characteristics.Bit32Machine),
            dllCharacteristics = (DllCharacteristics.HighEntropyVirtualAddressSpace ||| DllCharacteristics.DynamicBase |||
                DllCharacteristics.NxCompatible ||| DllCharacteristics.NoSeh ||| DllCharacteristics.TerminalServerAware)
        ),
        metadataRootBuilder,
        methodBodyBuilder.Builder,
        deterministicIdProvider = System.Func<_, _>(deterministicIdProvider)
    )

    portableExecutableBuilder.Serialize(builder)
    |> ignore // TODO: Should the content ID be saved somewhere?

let compileToStream options webAssemblyModule (stream: System.IO.Stream) =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanWrite then invalidArg (nameof stream) "Destination stream must support writing"
        let builder = BlobBuilder()
        compileToBlobBuilder options webAssemblyModule builder
        builder.WriteContentTo(stream)
    finally
        stream.Close()
