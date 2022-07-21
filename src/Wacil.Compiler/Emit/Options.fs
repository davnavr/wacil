namespace Wacil.Compiler.Emit

open System.Runtime.CompilerServices

[<IsReadOnly; Struct; RequireQualifiedAccess; NoComparison; StructuralEquality>]
type OutputType =
    | Assembly of System.Version
    | Module

    /// Gets the file extension used by files of this type, including the leading period.
    member this.FileExtension =
        match this with
        | Assembly _ -> ".dll"
        | Module -> ".netmodule"

[<IsReadOnly; Struct; RequireQualifiedAccess; NoComparison; StructuralEquality>]
type TargetFramework =
    | Net6

    member this.FrameworkName =
        match this with
        | Net6 -> ".NETCoreApp,Version=v6.0"

[<NoComparison; StructuralEquality>]
type Options =
    { TargetFramework: TargetFramework
      OutputType: OutputType
      Name: string
      RuntimeVersion: System.Version
      ///// <summary>If set, indicates that the module name should be obtained from the custom <c>name</c> section.</summary>
      //InferName: bool
      /// Indicates the name of the namespace containing the generated class.
      Namespace: string }
