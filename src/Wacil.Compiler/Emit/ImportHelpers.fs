[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.ImportHelpers

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures

let importType (importer: ReferenceImporter) (assembly: AssemblyReference) ns =
    fun name -> assembly.CreateTypeReference(ns, name) |> importer.ImportTypeOrNull

let importMember (importer: ReferenceImporter) signature name (parent: IMemberRefParent) =
    parent.CreateMemberReference(name, signature) |> importer.ImportMethodOrNull

let importMethod importer cconv returnType parameterTypes name parent =
    importMember importer (MethodSignature(cconv, returnType, parameterTypes)) name parent

let importField importer (signature: FieldSignature) name parent =
    importMember importer signature name parent

let importPropertyAccessor importer cconv propertyType name parent =
    importMethod importer cconv propertyType Seq.empty name parent

let importConstructor (mdle: ModuleDefinition) parameterTypes parent =
    importMethod
        mdle.DefaultImporter
        CallingConventionAttributes.HasThis
        mdle.CorLibTypeFactory.Void parameterTypes
        ".ctor"
        parent
