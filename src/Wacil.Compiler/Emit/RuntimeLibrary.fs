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
    { Type: ITypeDefOrRef
      Signature: TypeSignature
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
      ReadInt32: MethodSpecification
      WriteInt32: MethodSpecification
      Grow: MethodSpecification
      WriteArray: MethodSpecification }

[<NoComparison; NoEquality>]
type TableHelpersClass =
    { GetFunction: TypeSignature -> IMethodDescriptor }

/// <summary>Represents the references to the runtime library (<c>Wacil.Runtime.dll</c>).</summary>
[<NoComparison; NoEquality>]
type References =
    { UnreachableExceptionConstructor: IMethodDefOrRef
      Limits: LimitsClass
      Table: ITypeDefOrRef
      TableHelpers: TableHelpersClass
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
    let tyLimits = importRuntimeType "Limits"
    let tyMemoryHelpers = importRuntimeType "MemoryHelpers"
    let tyTable1 = importRuntimeType "Table`1"
    let tyTableHelpers = importRuntimeType "TableHelpers"
    let specFunctionTable = tyTable1.MakeGenericInstanceType syslib.MulticastDelegate.Signature
    let tyGlobal1 = importRuntimeType "Global`1"
    let tyGlobalHelpers = importRuntimeType "GlobalHelpers"

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
        let constructorParameterTypes = [| TypeDefOrRefSignature tyLimits :> TypeSignature |]
        let helperTypeParameter = GenericParameterSignature(GenericParameterType.Method, 0)

        let readInt32Template =
            ImportHelpers.importMethod
                mdle.DefaultImporter
                CallingConventionAttributes.Generic
                mdle.CorLibTypeFactory.Int32
                [| mdle.CorLibTypeFactory.Int32; helperTypeParameter; mdle.CorLibTypeFactory.Int32; mdle.CorLibTypeFactory.Byte |]
                "ReadInt32"
                tyMemoryHelpers

        readInt32Template.Signature.GenericParameterCount <- 1

        let createWriteTemplate ty =
            let reference =
                ImportHelpers.importMethod
                    mdle.DefaultImporter
                    CallingConventionAttributes.Generic
                    mdle.CorLibTypeFactory.Void
                    [| mdle.CorLibTypeFactory.Int32; ty; helperTypeParameter; mdle.CorLibTypeFactory.Int32; mdle.CorLibTypeFactory.Byte |]
                    "Write"
                    tyMemoryHelpers
            reference.Signature.GenericParameterCount <- 1
            reference

        let writeInt32Template = createWriteTemplate mdle.CorLibTypeFactory.Int32

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
                "Grow"
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
                        | _ ->
                            Some(ImportHelpers.importConstructor mdle constructorParameterTypes memoryTypeReference)
                      ReadInt32 = readInt32Template.MakeGenericInstanceMethod memoryTypeArguments
                      WriteInt32 = writeInt32Template.MakeGenericInstanceMethod memoryTypeArguments
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
        { LimitsClass.Type = tyLimits
          LimitsClass.Signature = TypeDefOrRefSignature tyLimits
          LimitsClass.Constructor =
            ImportHelpers.importConstructor
                mdle
                [| mdle.CorLibTypeFactory.Int32; mdle.CorLibTypeFactory.Int32 |]
                tyLimits }
      InstantiatedMemory = memoryInstanceFactory
    //   Memory =
    //     { Type = tyMemory
    //       Signature = sigMemory
    //       FieldSignature = FieldSignature sigMemory
    //       Constructor =
    //         ImportHelpers.importConstructor
    //             mdle
    //             [| mdle.CorLibTypeFactory.Int32; mdle.CorLibTypeFactory.Int32 |]
    //             tyMemory
    //       ReadInt32 =
    //         ImportHelpers.importMethod
    //             mdle.DefaultImporter
    //             CallingConventionAttributes.Default
    //             mdle.CorLibTypeFactory.Int32
    //             [| mdle.CorLibTypeFactory.UInt32; sigMemory; mdle.CorLibTypeFactory.UInt32; mdle.CorLibTypeFactory.Byte |]
    //             "ReadInt32"
    //             tyMemory
    //       WriteInt32 =
    //         ImportHelpers.importMethod
    //             mdle.DefaultImporter
    //             CallingConventionAttributes.Default
    //             mdle.CorLibTypeFactory.Void
    //             [|
    //                 mdle.CorLibTypeFactory.UInt32
    //                 mdle.CorLibTypeFactory.Int32
    //                 sigMemory
    //                 mdle.CorLibTypeFactory.UInt32
    //                 mdle.CorLibTypeFactory.Byte
    //             |]
    //             "WriteInt32"
    //             tyMemory
    //       Grow =
    //         ImportHelpers.importMethod
    //             mdle.DefaultImporter
    //             CallingConventionAttributes.Default
    //             mdle.CorLibTypeFactory.Int32
    //             [| mdle.CorLibTypeFactory.Int32; sigMemory |]
    //             "Grow"
    //             tyMemory
    //       WriteArray =
    //         ImportHelpers.importMethod
    //             mdle.DefaultImporter
    //             CallingConventionAttributes.HasThis
    //             mdle.CorLibTypeFactory.Void
    //             [| mdle.CorLibTypeFactory.UInt32; SzArrayTypeSignature mdle.CorLibTypeFactory.Byte |]
    //             "Write"
    //             tyMemory
    //       }
      InstantiatedTable = tableInstanceFactory
      InstantiatedGlobal = globalInstanceFactory
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
