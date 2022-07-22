[<AutoOpen>]
module internal Wacil.Compiler.Emit.ModuleMembers

open AsmResolver.DotNet

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type FunctionMember =
    | Defined of MethodDefinition * Wacil.Compiler.Wasm.Format.FuncType
    | Imported of import: FieldDefinition * func: FieldDefinition * invoke: MethodDefinition

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type MemoryMember =
    | Defined of memory: FieldDefinition
    | Imported of import: FieldDefinition * memory: FieldDefinition

[<NoComparison; NoEquality>]
type ModuleMembers =
    { Memories: MemoryMember[] }
