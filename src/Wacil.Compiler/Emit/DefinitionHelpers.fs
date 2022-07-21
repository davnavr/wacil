[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.DefinitionHelpers

open AsmResolver.PE.DotNet.Metadata.Tables.Rows;

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures

let addTypeDefinition (mdle: ModuleDefinition) btype flags tnamespace tname =
    let definition = TypeDefinition(tnamespace, tname, flags)
    definition.BaseType <- btype
    mdle.TopLevelTypes.Add definition
    definition

/// <summary>Defines a class deriving from <see cref="T:System.Object"/>.</summary>
let addNormalClass (syslib: SystemLibrary.References) mdle flags tnamespace tname =
    addTypeDefinition mdle syslib.Object.Type flags tnamespace tname

let addMethodDefinition (parent: TypeDefinition) signature flags name =
    let definition = MethodDefinition(name, flags, signature)
    parent.Methods.Add definition
    definition

let addInstanceConstructor parameters flags (parent: TypeDefinition) =
    addMethodDefinition
        parent
        (MethodSignature(CallingConventionAttributes.HasThis, parent.Module.CorLibTypeFactory.Void, parameters))
        (MethodAttributes.RuntimeSpecialName ||| MethodAttributes.SpecialName ||| flags)
        ".ctor"
