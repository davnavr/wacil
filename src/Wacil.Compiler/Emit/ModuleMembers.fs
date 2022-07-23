[<AutoOpen>]
module internal Wacil.Compiler.Emit.ModuleMembers

open AsmResolver.DotNet

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type FunctionMember =
    | Defined of MethodDefinition * Wacil.Compiler.Wasm.Format.FuncType
    | Imported of import: FieldDefinition * func: FieldDefinition * invoke: IMethodDefOrRef

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type MemoryMember =
    | Defined of memory: FieldDefinition
    | Imported of import: FieldDefinition * memory: FieldDefinition

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type GlobalMember =
    | Imported of import: FieldDefinition * variable: FieldDefinition

[<NoComparison; NoEquality>]
type ModuleMembers =
    { Functions: FunctionMember[]
      Memories: MemoryMember[]
      Globals: GlobalMember[] }
