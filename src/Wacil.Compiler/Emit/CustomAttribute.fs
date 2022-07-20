/// Helper module to apply custom attributes to members.
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.CustomAttribute

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures

let markCompilerGenerated (syslib: SystemLibrary.References) =
    let attribute = CustomAttribute(syslib.CompilerGeneratedAttributeConstructor, CustomAttributeSignature())
    fun (parent: IHasCustomAttribute) -> parent.CustomAttributes.Add attribute
