namespace Wacil.Compiler.Wasm.Validation

open System.Collections.Immutable

open Wacil.Compiler.Wasm

[<RequireQualifiedAccess; NoComparison; ReferenceEquality>]
type AnyFunction = Definition of int * Table.Function | Import of Table.FunctionImport

[<Sealed>]
type ValidModule =
    member CustomSections: ImmutableArray<Format.Custom>
    member Types: ImmutableArray<Format.FuncType>
    member Imports: Table.ModuleImportLookup
    member Functions: ImmutableArray<Table.Function>
    member Tables: ImmutableArray<Format.TableType>
    member Memories: ImmutableArray<Format.Limits>
    member Globals: ImmutableArray<Table.Global>
    member Exports: Table.ModuleExportLookup
    member Start: Table.Index voption
    member Data: ImmutableArray<Table.ValidData>
    
    member GetFunction: index: int -> AnyFunction

/// Base class used for all errors that occur during module validation.
[<Class>]
type ValidationException =
    inherit System.Exception

[<Sealed; Class>]
type DuplicateSectionException =
    inherit ValidationException

    member Section: Format.SectionId

[<Sealed; Class>]
type InvalidSectionOrderException =
    inherit ValidationException

    /// The section that was being parsed.
    member CurrentSection: Format.SectionId

    /// The section that should be placed before the current section.
    member NextSection: Format.SectionId

[<Sealed; Class>]
type FunctionSectionCountException =
    inherit ValidationException

    member Section: Format.SectionId
    member ExpectedCount: int
    member ActualCount: int

/// Thrown when the number of data segments is not equal to the data count.
[<Sealed; Class>]
type DataSegmentCountException =
    inherit ValidationException

    member Expected: int
    member Actual: int

[<Sealed; Class>]
type DuplicateExportException =
    inherit ValidationException

    member Name: string

[<Sealed; Class>]
type OperandStackUnderflowException = inherit ValidationException

[<Sealed; Class>]
type OperandTypeMismatchException =
    inherit ValidationException

    member Expected: Table.OperandType
    member Actual: Table.OperandType

[<Sealed; Class>]
type ControlFrameStackUnderflowException = inherit ValidationException

[<Sealed; Class>]
type ElseInstructionMismatchException =
    inherit ValidationException

    /// <summary>The index of the <c>else</c> instruction.</summary>
    member Index: int

    member PreviousStructuredInstruction: Format.Instruction

[<Sealed; Class>]
type GlobalIsNotMutableException =
    inherit ValidationException

    /// <summary>The index of the global.</summary>
    member Index: Format.Index

[<Sealed; Class>]
type TableElementTypeMismatchException =
    inherit ValidationException

    member Table: Format.Index
    member Expected: Format.RefType
    member Actual: Format.RefType

[<RequireQualifiedAccess>]
module Validate =
    /// <summary>Performs validation on a WebAssembly module containing the given <paramref name="sections"/>.</summary>
    /// <exception cref="T:Wacil.Compiler.Wasm.Validation.ValidationException">Thrown when validation fails.</exception>
    /// <seealso href="https://webassembly.github.io/spec/core/appendix/algorithm.html">WebAssembly Validation Algorithm</seealso>
    val fromModuleSections: sections: ImmutableArray<Format.Section> -> ValidModule
