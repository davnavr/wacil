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
      /// The name of the produced .NET module and/or assembly.
      OutputName: string // TODO: Allow retrieval of module name from WASM name section
      /// <summary>Indicates the version of the <c>Wacil.Runtime</c> library being referenced.</summary>
      RuntimeVersion: System.Version
      /// <summary>
      /// The name of the generated class corresponding to the WASM module. Defaults to the
      /// <see cref="P:Wacil.Compiler.Emit.Options.OutputName"/>.
      /// </summary>
      MainClassName: string
      /// Indicates the name of the namespace containing the classes generated during compilation.
      Namespace: string }
