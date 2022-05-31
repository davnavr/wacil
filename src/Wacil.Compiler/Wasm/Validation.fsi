namespace Wacil.Compiler.Wasm.Validation

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open Wacil.Compiler.Wasm

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type FunctionImport = { Name: string; Type: Format.FuncType }

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type TableImport = { Name: string; Type: Format.TableType }

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type MemoryImport = { Name: string; Limits: Format.Limits }

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type GlobalImport = { Name: string; Type: Format.GlobalType }

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type ModuleImports =
    { Functions: ImmutableArray<FunctionImport>
      Tables: ImmutableArray<TableImport>
      Memories: ImmutableArray<MemoryImport>
      Globals: ImmutableArray<GlobalImport> }

[<Sealed>]
type ModuleImportLookup =
    member Item: moduleImportName: string -> ModuleImports with get

    interface IReadOnlyDictionary<string, ModuleImports>

type ValidInstructionSequence = ImmutableArray<ValidInstruction>

and [<NoComparison; StructuralEquality>] ValidInstructionKind =
    | Normal
    | Branching of indices: ImmutableArray<int>
    | Structured of labels: ImmutableArray<int> * ImmutableArray<ValidInstructionSequence>
    
and [<NoComparison; StructuralEquality>] ValidInstruction =
    { Index: int
      PoppedTypes: ImmutableArray<Format.ValType>
      PushedTypes: ImmutableArray<Format.ValType>
      Instruction: Format.Instruction
      Kind: ValidInstructionKind }

[<NoComparison; StructuralEquality>]
type ValidExpression =
    internal
        { Source: Format.Expression
          mutable BranchTargets: ImmutableArray<int>
          mutable Expression: ValidInstructionSequence }

    member Instructions: ValidInstructionSequence
    member BranchTargetIndices: ImmutableArray<int>

[<NoComparison; StructuralEquality>]
type Function =
    { Type: Format.FuncType
      LocalTypes: ImmutableArray<Format.ValType>
      Body: ValidExpression }

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ModuleExport =
    | Function of Function
    | Table
    | Memory of Format.Index
    | Global

[<Sealed>]
type ModuleExportLookup =
    member GetMemoryName: index: Format.Index * name: outref<string> -> bool
    member GetFunctionName: index: Format.Index * name: outref<string> -> bool
    member Item: name: string -> ModuleExport with get

[<Sealed>]
type ValidModule =
    member CustomSections: ImmutableArray<Format.Custom>
    member Types: ImmutableArray<Format.FuncType>
    member Imports: ModuleImportLookup
    member Functions: ImmutableArray<Function>
    member Memories: ImmutableArray<Format.Limits>
    member Exports: ModuleExportLookup
    member Start: int voption

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type Error =
    | MultiMemoryNotSupported
    | DuplicateSection of id: Format.SectionId
    | InvalidSectionOrder of section: Format.SectionId * next: Format.SectionId
    | FunctionSectionCountMismatch of section: Format.SectionId * expectedCount: int * actualCount: int

    override ToString: unit -> string

[<RequireQualifiedAccess>]
module Validate =
    val fromModuleSections: sections: ImmutableArray<Format.Section> -> Result<ValidModule, Error>
