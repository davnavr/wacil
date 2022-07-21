[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.DefinitionHelpers

open AsmResolver.PE.DotNet.Metadata.Tables.Rows
open AsmResolver.PE.DotNet.Cil;

open AsmResolver.DotNet
open AsmResolver.DotNet.Code.Cil
open AsmResolver.DotNet.Signatures

let addTypeDefinition (mdle: ModuleDefinition) btype flags tnamespace tname =
    let definition = TypeDefinition(tnamespace, tname, flags)
    definition.BaseType <- btype
    mdle.TopLevelTypes.Add definition
    definition

/// <summary>Defines a class deriving from <see cref="T:System.Object"/>.</summary>
let addNormalClass (syslib: SystemLibrary.References) mdle flags tnamespace tname =
    addTypeDefinition mdle syslib.Object.Type flags tnamespace tname

let addFieldDefinition (parent: TypeDefinition) (signature: FieldSignature) flags name =
    let definition = FieldDefinition(name, flags, signature)
    parent.Fields.Add definition
    definition

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

let addPropertyDefinition (parent: TypeDefinition) signature flags name =
    let definition = PropertyDefinition(name, flags, signature)
    parent.Properties.Add definition
    definition

let addInstanceFieldGetter parent ty pflags mflags field (name: string) =
    let property =
        addPropertyDefinition parent (PropertySignature(CallingConventionAttributes.HasThis, ty, Seq.empty)) pflags name

    let getter =
        addMethodDefinition
            parent
            (MethodSignature(CallingConventionAttributes.HasThis, ty, Seq.empty))
            (MethodAttributes.SpecialName ||| mflags)
            ("get_" + name)

    property.SetSemanticMethods(getter, null)

    getter.CilMethodBody <- CilMethodBody getter
    let il = getter.CilMethodBody.Instructions
    il.Add(CilInstruction CilOpCodes.Ldarg_0)
    il.Add(CilInstruction(CilOpCodes.Ldfld, field))
    il.Add(CilInstruction CilOpCodes.Ret)
