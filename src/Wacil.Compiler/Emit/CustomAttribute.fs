/// Helper module to apply custom attributes to members.
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.CustomAttribute

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures

type Marker = IHasCustomAttribute -> unit

let private emptyAttributeSignature = CustomAttributeSignature()

let markCompilerGenerated (syslib: SystemLibrary.References): Marker =
    fun parent ->
        parent.CustomAttributes.Add(CustomAttribute(syslib.CompilerGeneratedAttributeConstructor, emptyAttributeSignature))

let markImportConstructor (rtlib: RuntimeLibrary.References): Marker =
    fun parent ->
        parent.CustomAttributes.Add(CustomAttribute(rtlib.ImportConstructorAttribute, emptyAttributeSignature))

let markCustomName (corLibTypes: Types.CorLibTypeFactory) (rtlib: RuntimeLibrary.References): string -> Marker =
    fun name parent ->
        let signature = CustomAttributeSignature([| CustomAttributeArgument(corLibTypes.String, name) |])
        parent.CustomAttributes.Add(CustomAttribute(rtlib.CustomNameAttribute, signature))
