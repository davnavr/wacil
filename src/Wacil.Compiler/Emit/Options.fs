namespace Wacil.Compiler.Emit

open System.Runtime.CompilerServices

[<IsReadOnly; Struct; RequireQualifiedAccess; NoComparison; StructuralEquality>]
type OutputType =
    | Assembly
    | Module

    /// Gets the file extension used by files of this type, including the leading period.
    member this.FileExtension =
        match this with
        | Assembly -> ".dll"
        | Module -> ".netmodule"

[<IsReadOnly; Struct; RequireQualifiedAccess; NoComparison; StructuralEquality>]
type TargetFramework =
    | Net6

[<NoComparison; StructuralEquality>]
type Options =
    { Name: string
      TargetFramework: TargetFramework
      OutputType: OutputType
      ///// <summary>If set, indicates that the module name should be obtained from the custom <c>name</c> section.</summary>
      //InferName: bool
      /// Indicates the name of the namespace containing the generated class.
      Namespace: string }
