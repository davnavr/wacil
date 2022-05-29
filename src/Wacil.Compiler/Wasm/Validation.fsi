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

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type ValidExpression =
    internal
    | Expr of ImmutableArray<Format.Instruction>

    member Instructions: ImmutableArray<Format.Instruction>

[<NoComparison; StructuralEquality>]
type Function =
    { Type: Format.FuncType
      Body: ValidExpression }

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ModuleExport =
    | Function of Function
    | Table
    | Memory of Format.Index
    | Global

[<Sealed>]
type ModuleExportLookup =
    member GetMemoryName: index: Format.Index -> string
    member Item: name: string -> ModuleExport with get

[<Sealed>]
type ValidModule =
    member CustomSections: ImmutableArray<Format.Custom>
    member Types: ImmutableArray<Format.FuncType>
    member Imports: ModuleImportLookup
    member Functions: ImmutableArray<Function>
    member Memories: ImmutableArray<Format.Limits>
    member Exports: ModuleExportLookup

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
