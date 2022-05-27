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

[<Sealed>]
type ValidModule =
    member CustomSections: ImmutableArray<Format.Custom>
    member Types: ImmutableArray<Format.FuncType>
    member Imports: ModuleImportLookup
    
    member Memories: ImmutableArray<Format.Limits>

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type Error =
    | MultiMemoryNotSupported
    | DuplicateSection of id: Format.SectionId
    | InvalidSectionOrder of section: Format.SectionId * next: Format.SectionId

    override ToString: unit -> string

[<RequireQualifiedAccess>]
module Validate =
    val fromModuleSections: sections: ImmutableArray<Format.Section> -> Result<ValidModule, Error>
