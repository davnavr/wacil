/// <summary>Module for generating references to classes in the <c>Wacil.Runtime</c> assembly.</summary>
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.RuntimeLibrary

open Wacil.Compiler

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types

/// <summary>Represents an instantiation of the <c>Wacil.Runtime.Table</c> class.</summary>
[<NoComparison; NoEquality>]
type TableInstantiation =
    { Instantiation: GenericInstanceTypeSignature
      Specification: TypeSpecification
      Constructor: IMethodDefOrRef
      Get: IMethodDefOrRef }

[<NoComparison; NoEquality>]
type MemoryClass =
    { Type: ITypeDefOrRef
      Signature: TypeSignature
      Constructor: IMethodDefOrRef
      ReadInt32: IMethodDefOrRef
      WriteInt32: IMethodDefOrRef
      Grow: IMethodDefOrRef
      WriteArray: IMethodDefOrRef }

/// <summary>Represents the references to the runtime library (<c>Wacil.Runtime.dll</c>).</summary>
[<NoComparison; NoEquality>]
type References =
    { UnreachableExceptionConstructor: IMethodDefOrRef
      Memory: MemoryClass
      Table: ITypeDefOrRef
      /// <summary>Instantiates the <c>Wacil.Runtime.Table</c> class for a given element type.</summary>
      InstantiatedTable: Wasm.Format.RefType -> TableInstantiation }

let importTypes runtimeLibraryVersion wasmTypeTranslator (mscorlib: SystemLibrary.References) (mdle: ModuleDefinition) =
    let name = "Wacil.Runtime"
    let assembly = AssemblyReference(name, runtimeLibraryVersion)
    mdle.AssemblyReferences.Add assembly

    let importRuntimeType = ImportHelpers.importType mdle.DefaultImporter assembly name

    let tyUnreachableException = importRuntimeType "UnreachableException"
    let tyMemory = importRuntimeType "Memory"
    let tyTable1 = importRuntimeType "Table`1"

    let sigMemory = TypeDefOrRefSignature tyMemory

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
                    { Instantiation = instantiation
                      Specification = specification
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
                        }
                    |> ValueSome

            container.Value.Value

    { UnreachableExceptionConstructor =
        ImportHelpers.importConstructor
            mdle
            Seq.empty
            tyUnreachableException
      Table = tyTable1
      Memory =
        { Type = tyMemory
          Signature = sigMemory
          Constructor =
            ImportHelpers.importConstructor
                mdle
                [| mdle.CorLibTypeFactory.Int32; mdle.CorLibTypeFactory.Int32 |]
                tyMemory
          ReadInt32 =
            ImportHelpers.importMethod
                mdle.DefaultImporter
                CallingConventionAttributes.Default
                mdle.CorLibTypeFactory.Int32
                [| mdle.CorLibTypeFactory.UInt32; sigMemory; mdle.CorLibTypeFactory.UInt32; mdle.CorLibTypeFactory.Byte |]
                "ReadInt32"
                tyMemory
          WriteInt32 =
            ImportHelpers.importMethod
                mdle.DefaultImporter
                CallingConventionAttributes.Default
                mdle.CorLibTypeFactory.Void
                [|
                    mdle.CorLibTypeFactory.UInt32
                    mdle.CorLibTypeFactory.Int32
                    sigMemory
                    mdle.CorLibTypeFactory.UInt32
                    mdle.CorLibTypeFactory.Byte
                |]
                "WriteInt32"
                tyMemory
          Grow =
            ImportHelpers.importMethod
                mdle.DefaultImporter
                CallingConventionAttributes.Default
                mdle.CorLibTypeFactory.Int32
                [| mdle.CorLibTypeFactory.Int32; sigMemory |]
                "Grow"
                tyMemory
          WriteArray =
            ImportHelpers.importMethod
                mdle.DefaultImporter
                CallingConventionAttributes.HasThis
                mdle.CorLibTypeFactory.Void
                [| mdle.CorLibTypeFactory.UInt32; SzArrayTypeSignature mdle.CorLibTypeFactory.Byte |]
                "Write"
                tyMemory
          }
      InstantiatedTable = tableInstanceFactory }
