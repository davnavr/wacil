[<AutoOpen>]
module internal Wacil.Compiler.Emit.ModuleMembers

open Wacil.Compiler

open AsmResolver.DotNet

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type FunctionMember =
    | Defined of instance: MethodDefinition * indirect: MethodDefinition * Wasm.Format.FuncType
    | Imported of import: FieldDefinition * func: FieldDefinition * invoke: IMethodDefOrRef * indirect: MethodDefinition *
        Wasm.Format.FuncType

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type MemoryMember =
    | Defined of memory: FieldDefinition
    | Imported of import: FieldDefinition * memory: FieldDefinition

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type GlobalMember =
    /// <summary>
    /// <p>Represents a WebAssembly global variable that is not exported.</p>
    /// <p>The compiler can optimize in this case, and can generate a field to directly contain the value.</p>
    /// </summary>
    | Defined of FieldDefinition * setter: MethodDefinition voption
    | DefinedExport of container: FieldDefinition * accessor: IMethodDescriptor * setter: IMethodDescriptor
    | Imported of import: FieldDefinition * variable: FieldDefinition * accessor: IMethodDescriptor * setter: IMethodDescriptor

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type DataSegmentMember =
    | Passive of data: FieldDefinition
    | Active

[<NoComparison; NoEquality>]
type ModuleMembers =
    { Functions: FunctionMember[]
      Memories: MemoryMember[]
      Globals: GlobalMember[]
      DataSegments: DataSegmentMember[] }
