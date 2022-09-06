/// <summary>Module for generating references to vector classes in the <c>Wacil.Runtime</c> assembly.</summary>
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.VectorLibrary

open Wacil.Compiler

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types

[<NoComparison; NoEquality>]
type References =
    { Type: ITypeDefOrRef
      ConstructorI64: IMethodDefOrRef
      ConstructorI32: IMethodDefOrRef
      ConstructorI16: IMethodDefOrRef
      AddI64: IMethodDefOrRef }

let importTypes (runtimeLibrary: RuntimeLibrary.References) (mdle: ModuleDefinition) =
    let vectorTypeReference = ImportHelpers.importType mdle.DefaultImporter runtimeLibrary.Assembly "Wacil.Runtime" "Vector128"
    let vectorTypeSignature = TypeDefOrRefSignature vectorTypeReference
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

    { Type = vectorTypeReference
      ConstructorI64 = importConstructor (Array.create 2 coreLibraryTypes.Int64)
      ConstructorI32 = importConstructor (Array.create 4 coreLibraryTypes.Int32)
      ConstructorI16 = importConstructor (Array.create 8 coreLibraryTypes.Int16)
      AddI64 = importBinaryOperation "AddInt64" }
