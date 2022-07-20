[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.ImportHelpers

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures

let importType (importer: ReferenceImporter) (assembly: AssemblyReference) ns =
    fun name -> assembly.CreateTypeReference(ns, name) |> importer.ImportTypeOrNull

let importMethod (importer: ReferenceImporter) cconv returnType parameterTypes name (parent: IMemberRefParent) =
    parent.CreateMemberReference(name, MethodSignature(cconv, returnType, parameterTypes)) |> importer.ImportMethodOrNull

let importPropertyAccessor importer cconv propertyType name parent =
    importMethod importer cconv propertyType Seq.empty name parent

let importConstructor (mdle: ModuleDefinition) parameterTypes parent =
    importMethod
        mdle.DefaultImporter
        CallingConventionAttributes.HasThis
        mdle.CorLibTypeFactory.Void parameterTypes
        ".ctor"
        parent
