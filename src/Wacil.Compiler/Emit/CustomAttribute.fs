/// Helper module to apply custom attributes to members.
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.CustomAttribute

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures

type Marker = IHasCustomAttribute -> unit

let markCompilerGenerated (syslib: SystemLibrary.References): Marker =
    let signature = CustomAttributeSignature()
    fun parent -> parent.CustomAttributes.Add(CustomAttribute(syslib.CompilerGeneratedAttributeConstructor, signature))
