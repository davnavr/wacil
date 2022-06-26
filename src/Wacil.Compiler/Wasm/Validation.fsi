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
    member Functions: ImmutableArray<struct(string * FunctionImport)>

    member Count: int

    interface IReadOnlyDictionary<string, ModuleImports>

[<NoComparison; StructuralEquality>]
type IntroducedLabel =
    internal { mutable Value: int voption }

    member Index: int

[<NoComparison; StructuralEquality>]
type IntroducedLabelLookup =
    internal
        { Lookup: Dictionary<Format.Index, IntroducedLabel>
          Labels: ImmutableArray<IntroducedLabel> }

    member TryGetLabel: index: Format.Index * label: outref<IntroducedLabel> -> bool

    member GetLabel: index: Format.Index -> IntroducedLabel

    static member Empty: IntroducedLabelLookup

type ValidInstructionSequence = ImmutableArray<ValidInstruction>

and [<NoComparison; StructuralEquality>] ValidInstructionKind =
    | Normal
    | Structured of IntroducedLabelLookup * ImmutableArray<ValidInstructionSequence>

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
          mutable Expression: ValidInstructionSequence
          mutable BranchTargets: ImmutableArray<int> }

    member Instructions: ValidInstructionSequence

    /// The indices of all instructions that control flow will transfer to when a branch is taken, listed in increasing order.
    member LabelIndices: ImmutableArray<int>

[<NoComparison; StructuralEquality>]
type Function =
    { Type: Format.FuncType
      LocalTypes: ImmutableArray<Format.ValType>
      Body: ValidExpression }

[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type Global =
    { Type: Format.GlobalType
      Value: ValidExpression }

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
    member GetTableName: index: Format.Index * name: outref<string> -> bool
    member Item: name: string -> ModuleExport with get

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ValidActiveData =
    { Memory: Format.Index
      /// An expression that evaluates to an offset that the data is copied to.
      Offset: ValidExpression }

[<System.Obsolete>]
[<RequireQualifiedAccess; NoComparison; NoEquality>]
type ValidData =
    { Bytes: ImmutableArray<byte>
      Mode: ValidActiveData voption }

[<System.Obsolete>]
[<Sealed>]
type ValidModule =
    member CustomSections: ImmutableArray<Format.Custom>
    member Types: ImmutableArray<Format.FuncType>
    member Imports: ModuleImportLookup
    member Functions: ImmutableArray<Function>
    member Tables: ImmutableArray<Format.TableType>
    member Memories: ImmutableArray<Format.Limits>
    member Globals: ImmutableArray<Global>
    member Exports: ModuleExportLookup
    member Start: int voption
    member Data: ImmutableArray<ValidData>

[<System.Obsolete>]
[<RequireQualifiedAccess; NoComparison; StructuralEquality>]
type Error =
    | MultiMemoryNotSupported
    | DuplicateSection of id: Format.SectionId
    | InvalidSectionOrder of section: Format.SectionId * next: Format.SectionId
    | FunctionSectionCountMismatch of section: Format.SectionId * expectedCount: int * actualCount: int

    override ToString: unit -> string

[<System.Obsolete>]
[<RequireQualifiedAccess>]
module Validate =
    val fromModuleSections: sections: ImmutableArray<Format.Section> -> Result<ValidModule, Error>
