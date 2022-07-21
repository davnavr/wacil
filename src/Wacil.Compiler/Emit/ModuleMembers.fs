[<AutoOpen>]
module internal Wacil.Compiler.Emit.ModuleMembers

open AsmResolver.DotNet

[<NoComparison; NoEquality>]
type MemoryMember =
    | Defined of memory: FieldDefinition
    | Imported of import: FieldDefinition * memory: FieldDefinition

[<NoComparison; NoEquality>]
type ModuleMembers =
    { Memories: MemoryMember[] }
