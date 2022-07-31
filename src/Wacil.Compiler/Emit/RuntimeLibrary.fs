/// <summary>Module for generating references to classes in the <c>Wacil.Runtime</c> assembly.</summary>
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.RuntimeLibrary

open System.Collections.Generic

open Wacil.Compiler

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type LimitsClass =
    { Signature: TypeSignature
      Constructor: IMethodDefOrRef }

/// <summary>Represents an instantiation of the <c>Wacil.Runtime.Table&lt;T&gt;</c> class for a particular element type.</summary>
[<NoComparison; NoEquality>]
type TableInstantiation =
    { Instantiation: GenericInstanceTypeSignature
      Specification: TypeSpecification
      FieldSignature: FieldSignature
      Constructor: IMethodDefOrRef
      /// <summary>The <c>Get</c> method used to obtain the element at a specific index</summary>
      Get: IMethodDefOrRef
      Initialize: IMethodDefOrRef }

/// <summary>Represents an instantiation of the <c>Wacil.Runtime.Global&lt;T&gt;</c> class.</summary>
[<NoComparison; NoEquality>]
type GlobalInstantiation =
    { Instantiation: GenericInstanceTypeSignature
      Specification: TypeSpecification
      FieldSignature: FieldSignature
      Constructor: IMethodDefOrRef
      /// <summary>
      /// The <c>get_Mutable</c> instance method that returns a boolean that is true if the global variable is mutable.
      /// </summary>
      MutableAccessor: IMethodDefOrRef
      /// <summary>
      /// The <c>get_Value</c> instance method used to retrieve the value of a global variable.
      /// </summary>
      ValueAccessor: IMethodDefOrRef
      /// <summary>
      /// The <c>Wacil.Runtime.GlobalHelpers.SetValue&lt;T&gt;</c> helper method used to set the value
      /// of a global variable.
      /// </summary>
      SetValueHelper: MethodSpecification }

[<NoComparison; NoEquality>]
type MemoryInstantiation =
    { Type: ITypeDefOrRef
      Signature: TypeSignature
      FieldSignature: FieldSignature
      Constructor: IMethodDefOrRef option
      PageCount: IMethodDefOrRef
      ReadByte: MethodSpecification
      ReadInt16: MethodSpecification
      ReadInt32: MethodSpecification
      ReadInt64: MethodSpecification
      WriteByte: MethodSpecification
      WriteInt16: MethodSpecification
      WriteInt32: MethodSpecification
      WriteInt64: MethodSpecification
      Grow: MethodSpecification
      WriteArray: MethodSpecification }

    member this.UsesVirtualCalls = this.Constructor.IsNone

[<NoComparison; NoEquality>]
type TableHelpersClass =
    { GetFunction: TypeSignature -> IMethodDescriptor }

[<NoComparison; NoEquality>]
type IntegerHelpersClass =
    { RotateLeftInt32: IMethodDefOrRef
      RotateLeftInt64: IMethodDefOrRef
      RotateRightInt32: IMethodDefOrRef
      RotateRightInt64: IMethodDefOrRef }

/// <summary>Represents the references to the runtime library (<c>Wacil.Runtime.dll</c>).</summary>
[<NoComparison; NoEquality>]
type References =
    { UnreachableExceptionConstructor: IMethodDefOrRef
      Limits: LimitsClass
      Table: ITypeDefOrRef
      TableHelpers: TableHelpersClass
      IntegerHelpers: IntegerHelpersClass
      /// <summary>Instantiates the <c>Wacil.Runtime.Table&lt;T&gt;</c> class for a given element type.</summary>
      InstantiatedTable: Wasm.Format.RefType -> TableInstantiation
      /// <summary>Instantiates the <c>Wacil.Runtime.Global&lt;T&gt;</c> class.</summary>
      InstantiatedGlobal: Wasm.Format.ValType -> GlobalInstantiation
      InstantiatedMemory: MemoryImplementation -> MemoryInstantiation }

let importTypes runtimeLibraryVersion wasmTypeTranslator (syslib: SystemLibrary.References) (mdle: ModuleDefinition) =
    let name = "Wacil.Runtime"
    let assembly = AssemblyReference(name, runtimeLibraryVersion)
    mdle.AssemblyReferences.Add assembly

    let importRuntimeType = ImportHelpers.importType mdle.DefaultImporter assembly name

    let tyUnreachableException = importRuntimeType "UnreachableException"
    let tyMemoryHelpers = importRuntimeType "MemoryHelpers"
    let tyTable1 = importRuntimeType "Table`1"
    let tyTableHelpers = importRuntimeType "TableHelpers"
    let specFunctionTable = tyTable1.MakeGenericInstanceType syslib.MulticastDelegate.Signature
    let tyGlobal1 = importRuntimeType "Global`1"
    let tyGlobalHelpers = importRuntimeType "GlobalHelpers"
    let tyIntegerHelpers = importRuntimeType "IntegerHelpers"

    let tyLimits = importRuntimeType "Limits"
    let sigLimits = TypeDefOrRefSignature(tyLimits, isValueType = true)

    let tableInstanceFactory =
        let funcRefTable = ref ValueNone
        let externRefTable = ref ValueNone

        let elementTypeParameter = GenericParameterSignature(GenericParameterType.Type, 0)
        let specTable1 = tyTable1.MakeGenericInstanceType elementTypeParameter

        fun ty ->
            let container =
                match ty with
                | Wasm.Format.ExternRef -> externRefTable
                | Wasm.Format.FuncRef -> funcRefTable

            if container.Value.IsNone then
                let translatedElementType: TypeSignature = wasmTypeTranslator (Wasm.Format.ValType.ofRefType ty)
                let instantiation = tyTable1.MakeGenericInstanceType [| translatedElementType |]
                let specification = TypeSpecification instantiation

                container.Value <-
                    { TableInstantiation.Instantiation = instantiation
                      Specification = specification
                      FieldSignature = FieldSignature instantiation
                      Constructor =
                        ImportHelpers.importConstructor
                            mdle
                            [| mdle.CorLibTypeFactory.Int32; mdle.CorLibTypeFactory.Int32 |]
                            specification
                      Get =
                        ImportHelpers.importMethod
                            mdle.DefaultImporter
                            CallingConventionAttributes.Default
                            elementTypeParameter
                            [| mdle.CorLibTypeFactory.Int32; specTable1 |]
                            "Get"
                            specification
                      Initialize =
                        ImportHelpers.importMethod
                            mdle.DefaultImporter
                            CallingConventionAttributes.Default
                            mdle.CorLibTypeFactory.Void
                            [|
                                mdle.CorLibTypeFactory.Int32
                                mdle.CorLibTypeFactory.Int32
                                mdle.CorLibTypeFactory.Int32
                                specTable1
                                elementTypeParameter.MakeSzArrayType()
                            |]
                            "Initialize"
                            specification }
                    |> ValueSome

            container.Value.Value

    let globalInstanceFactory =
        let helperSetValueTemplate =
            let helperValueTypeParameter = GenericParameterSignature(GenericParameterType.Method, 0)
            let helperGlobalType = tyGlobal1.MakeGenericInstanceType [| helperValueTypeParameter :> TypeSignature |]
            ImportHelpers.importMethod
                mdle.DefaultImporter
                CallingConventionAttributes.Generic
                mdle.CorLibTypeFactory.Void
                [| helperValueTypeParameter; helperGlobalType |]
                "SetValue"
                tyGlobalHelpers

        helperSetValueTemplate.Signature.GenericParameterCount <- 1

        let lookup = Dictionary()
        let globalValueTypeParameter = GenericParameterSignature(GenericParameterType.Type, 0)
        fun ty ->
            match lookup.TryGetValue ty with
            | true, existing -> existing
            | false, _ ->
                let translatedValueType = wasmTypeTranslator ty
                let instantiation = tyGlobal1.MakeGenericInstanceType [| translatedValueType |]
                let specification = TypeSpecification instantiation

                let instantiation' =
                    { GlobalInstantiation.Instantiation = instantiation
                      Specification = specification
                      FieldSignature = FieldSignature instantiation
                      Constructor =
                        ImportHelpers.importConstructor
                            mdle
                            [| globalValueTypeParameter; mdle.CorLibTypeFactory.Boolean |]
                            specification
                      MutableAccessor =
                        ImportHelpers.importMethod
                            mdle.DefaultImporter
                            CallingConventionAttributes.HasThis
                            mdle.CorLibTypeFactory.Boolean
                            Seq.empty
                            "get_Mutable"
                            specification
                      ValueAccessor =
                        ImportHelpers.importMethod
                            mdle.DefaultImporter
                            CallingConventionAttributes.HasThis
                            globalValueTypeParameter
                            Seq.empty
                            "get_Value"
                            specification
                      SetValueHelper = helperSetValueTemplate.MakeGenericInstanceMethod [| translatedValueType |] }

                lookup[ty] <- instantiation'
                instantiation'

    let memoryInstanceFactory =
        let constructorParameterTypes = [| sigLimits :> TypeSignature |]
        let helperTypeParameter = GenericParameterSignature(GenericParameterType.Method, 0)

        let createMemoryHelper returnType parameterTypes name =
            let reference =
                ImportHelpers.importMethod
                    mdle.DefaultImporter
                    CallingConventionAttributes.Generic
                    returnType
                    parameterTypes
                    name
                    tyMemoryHelpers
            reference.Signature.GenericParameterCount <- 1
            reference

        let createReadTemplate ty name =
            createMemoryHelper
                ty
                [| mdle.CorLibTypeFactory.Int32; helperTypeParameter; mdle.CorLibTypeFactory.Int32; mdle.CorLibTypeFactory.Byte |]
                name

        let readByteTemplate =
            createMemoryHelper
                mdle.CorLibTypeFactory.Byte
                [| mdle.CorLibTypeFactory.Int32; helperTypeParameter; mdle.CorLibTypeFactory.Int32; |]
                "ReadByte"

        let readInt16Template = createReadTemplate mdle.CorLibTypeFactory.Int16 "ReadInt16"
        let readInt32Template = createReadTemplate mdle.CorLibTypeFactory.Int32 "ReadInt32"
        let readInt64Template = createReadTemplate mdle.CorLibTypeFactory.Int64 "ReadInt64"

        let createWriteTemplate ty =
            createMemoryHelper
                mdle.CorLibTypeFactory.Void
                [| mdle.CorLibTypeFactory.Int32; ty; helperTypeParameter; mdle.CorLibTypeFactory.Int32; mdle.CorLibTypeFactory.Byte |]
                "Write"

        let writeByteTemplate =
            createMemoryHelper
                mdle.CorLibTypeFactory.Void
                [| mdle.CorLibTypeFactory.Int32; mdle.CorLibTypeFactory.Byte; helperTypeParameter; mdle.CorLibTypeFactory.Int32 |]
                "Write"

        let writeInt16Template = createWriteTemplate mdle.CorLibTypeFactory.Int16
        let writeInt32Template = createWriteTemplate mdle.CorLibTypeFactory.Int32
        let writeInt64Template = createWriteTemplate mdle.CorLibTypeFactory.Int64

        let growHelperTemplate =
            ImportHelpers.importMethod
                mdle.DefaultImporter
                CallingConventionAttributes.Generic
                mdle.CorLibTypeFactory.Int32
                [| mdle.CorLibTypeFactory.Int32; helperTypeParameter |]
                "Grow"
                tyMemoryHelpers

        growHelperTemplate.Signature.GenericParameterCount <- 1

        let writeArrayTemplate =
            ImportHelpers.importMethod
                mdle.DefaultImporter
                CallingConventionAttributes.Generic
                mdle.CorLibTypeFactory.Void
                [| helperTypeParameter; mdle.CorLibTypeFactory.Int32; mdle.CorLibTypeFactory.Byte.MakeSzArrayType() |]
                "Write"
                tyMemoryHelpers

        writeArrayTemplate.Signature.GenericParameterCount <- 1

        let lookup = Dictionary<MemoryImplementation, MemoryInstantiation>()
        fun impl ->
            match lookup.TryGetValue impl with
            | true, existing -> existing
            | false, _ ->
                let memoryTypeReference =
                    match impl with
                    | MemoryImplementation.Any -> "IMemory32"
                    | MemoryImplementation.Array -> "ArrayMemory"
                    | MemoryImplementation.Unmanaged -> "UnmanagedMemory"
                    | MemoryImplementation.Segmented -> "SegmentedMemory"
                    |> importRuntimeType

                let memoryTypeSignature = TypeDefOrRefSignature memoryTypeReference
                let memoryTypeArguments = [| memoryTypeSignature :> TypeSignature |]

                let instantiation =
                    { MemoryInstantiation.Type = memoryTypeReference
                      Signature = memoryTypeSignature
                      FieldSignature = FieldSignature memoryTypeSignature
                      Constructor =
                        match impl with
                        | MemoryImplementation.Any -> None
                        | _ -> Some(ImportHelpers.importConstructor mdle constructorParameterTypes memoryTypeReference)
                      PageCount =
                        ImportHelpers.importPropertyAccessor
                            mdle.DefaultImporter
                            CallingConventionAttributes.HasThis
                            mdle.CorLibTypeFactory.Int32
                            "get_PageCount"
                            memoryTypeReference
                      ReadByte = readByteTemplate.MakeGenericInstanceMethod memoryTypeArguments
                      ReadInt16 = readInt16Template.MakeGenericInstanceMethod memoryTypeArguments
                      ReadInt32 = readInt32Template.MakeGenericInstanceMethod memoryTypeArguments
                      ReadInt64 = readInt64Template.MakeGenericInstanceMethod memoryTypeArguments
                      WriteByte = writeByteTemplate.MakeGenericInstanceMethod memoryTypeArguments
                      WriteInt16 = writeInt16Template.MakeGenericInstanceMethod memoryTypeArguments
                      WriteInt32 = writeInt32Template.MakeGenericInstanceMethod memoryTypeArguments
                      WriteInt64 = writeInt64Template.MakeGenericInstanceMethod memoryTypeArguments
                      Grow = growHelperTemplate.MakeGenericInstanceMethod memoryTypeArguments
                      WriteArray = writeArrayTemplate.MakeGenericInstanceMethod memoryTypeArguments }

                lookup[impl] <- instantiation
                instantiation

    { UnreachableExceptionConstructor =
        ImportHelpers.importConstructor
            mdle
            Seq.empty
            tyUnreachableException
      Table = tyTable1
      Limits =
        { LimitsClass.Signature = sigLimits
          LimitsClass.Constructor =
            ImportHelpers.importConstructor
                mdle
                [| mdle.CorLibTypeFactory.Int32; mdle.CorLibTypeFactory.Int32 |]
                tyLimits }
      InstantiatedMemory = memoryInstanceFactory
      InstantiatedTable = tableInstanceFactory
      InstantiatedGlobal = globalInstanceFactory
      IntegerHelpers =
        let integerOperationHelper returnType parameterTypes name  =
            ImportHelpers.importMethod
                mdle.DefaultImporter
                CallingConventionAttributes.Default
                returnType
                parameterTypes
                name
                tyIntegerHelpers
        { RotateLeftInt32 =
            integerOperationHelper
                mdle.CorLibTypeFactory.Int32
                [| mdle.CorLibTypeFactory.Int32; mdle.CorLibTypeFactory.Int32 |]
                "RotateLeft"
          RotateLeftInt64 =
            integerOperationHelper
                mdle.CorLibTypeFactory.Int64
                [| mdle.CorLibTypeFactory.Int64; mdle.CorLibTypeFactory.Int64 |]
                "RotateLeft"
          RotateRightInt32 =
            integerOperationHelper
                mdle.CorLibTypeFactory.Int32
                [| mdle.CorLibTypeFactory.Int32; mdle.CorLibTypeFactory.Int32 |]
                "RotateRight"
          RotateRightInt64 =
            integerOperationHelper
                mdle.CorLibTypeFactory.Int64
                [| mdle.CorLibTypeFactory.Int64; mdle.CorLibTypeFactory.Int64 |]
                "RotateRight" }
      TableHelpers =
        let getFunctionTypeParameter = GenericParameterSignature(GenericParameterType.Method, 0)
        let getFunctionTemplate =
            ImportHelpers.importMethod
                mdle.DefaultImporter
                CallingConventionAttributes.Generic
                getFunctionTypeParameter
                [| mdle.CorLibTypeFactory.Int32; specFunctionTable |]
                "GetFunction"
                tyTableHelpers

        getFunctionTemplate.Signature.GenericParameterCount <- 1

        { GetFunction = fun tyDelegate -> getFunctionTemplate.MakeGenericInstanceMethod [| tyDelegate |] } }
