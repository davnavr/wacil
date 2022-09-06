/// <summary>Module for generating references to vector classes in the <c>Wacil.Runtime</c> assembly.</summary>
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.VectorLibrary

open Wacil.Compiler

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types

[<NoComparison; NoEquality>]
type References =
    { Signature: TypeSignature
      ConstructorElementsI64: IMethodDefOrRef
      ConstructorElementsI32: IMethodDefOrRef
      ConstructorElementsI16: IMethodDefOrRef
      AddI32: IMethodDefOrRef
      AddI64: IMethodDefOrRef }

let importTypes (runtimeLibrary: RuntimeLibrary.References) (mdle: ModuleDefinition) =
    let vectorTypeReference = ImportHelpers.importType mdle.DefaultImporter runtimeLibrary.Assembly "Wacil.Runtime" "Vector128"
    let vectorTypeSignature = TypeDefOrRefSignature(vectorTypeReference, isValueType = true)
    let coreLibraryTypes = mdle.CorLibTypeFactory

    let importConstructor parameterTypes = ImportHelpers.importConstructor mdle parameterTypes vectorTypeReference
    let importBinaryOperation name =
        ImportHelpers.importMethod
            mdle.DefaultImporter
            CallingConventionAttributes.HasThis
            vectorTypeSignature
            [| vectorTypeSignature |]
            name
            vectorTypeReference

    { Signature = vectorTypeSignature
      ConstructorElementsI64 = importConstructor (Array.create 2 coreLibraryTypes.Int64)
      ConstructorElementsI32 = importConstructor (Array.create 4 coreLibraryTypes.Int32)
      ConstructorElementsI16 = importConstructor (Array.create 8 coreLibraryTypes.Int16)
      AddI32 = importBinaryOperation "AddInt32"
      AddI64 = importBinaryOperation "AddInt64" }
