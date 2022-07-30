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

/// Indicates the class used when translating WebAssembly memories.
[<NoComparison; StructuralEquality>]
type MemoryImplementation =
    /// <summary>
    /// Indicates that the <c>Wacil.Runtime.IMemory32</c> interface is used, allowing any memory implementation at the cost of
    /// performance due to dynamic dispatch.
    /// </summary>
    | Any
    | Array
    | Segmented
    | Unmanaged

[<Sealed>]
type Options (name) =
    let mutable mainClassName = null

    member val TargetFramework = TargetFramework.Net6 with get, set

    member val OutputType = OutputType.Assembly(System.Version(1, 0, 0, 0)) with get, set

    member val OutputName = if System.String.IsNullOrEmpty name then "Module" else name

    /// <summary>Indicates the version of the <c>Wacil.Runtime</c> library being referenced.</summary>
    member val RuntimeVersion = System.Version(1, 0, 0, 0) with get, set

    /// <summary>
    /// The name of the generated class corresponding to the WASM module. Defaults to the
    /// <see cref="P:Wacil.Compiler.Emit.Options.OutputName"/>.
    /// </summary>
    member this.MainClassName
        with get() = if isNull mainClassName then this.OutputName else mainClassName
        and set name =
            if System.String.IsNullOrEmpty name then invalidArg (nameof name) "main class name must not be empty"
            mainClassName <- name

    /// Indicates the name of the namespace containing the classes generated during compilation.
    member val Namespace = System.String.Empty with get, set
