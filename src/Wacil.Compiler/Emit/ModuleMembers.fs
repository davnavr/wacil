[<AutoOpen>]
module internal Wacil.Compiler.Emit.ModuleMembers

open Wacil.Compiler.Helpers.Collections

open AsmResolver.DotNet

[<NoComparison; NoEquality>]
type MemoryMember =
    | Defined of memory: FieldDefinition
    | Imported of import: FieldDefinition * memory: FieldDefinition

[<NoComparison; NoEquality>]
type ModuleMembers =
    { mutable Memories: ArrayBuilder<MemoryMember> }
