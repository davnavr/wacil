/// <summary>Contains functions for parsing the contents of a WebAssembly custom name section.</summary>
/// <remarks>
/// Note that since the WebAssembly specification essentially says that errors in custom sections should be ignored, errors encountered
/// during parsing do not throw an exception.
/// </remarks>
[<RequireQualifiedAccess>]
module Wacil.Compiler.Wasm.CustomNames

type ParseException = Parser.ParseException

[<Sealed; Class>]
type Lookup =
    /// <summary>Gets the name, if any, that is assigned to the WebAssembly module.</summary>
    member ModuleName : Format.Name

    /// <summary>Gets the name corresponding to the function specified by the <paramref name="index"/>.</summary>
    member GetFunctionName : index: Format.FuncIdx -> Format.Name

    /// <summary>Gets the name of a local variable within the specified <paramref name="parent"/> function.</summary>
    member GetLocalName : parent: Format.FuncIdx * index: Format.LocalIdx -> Format.Name

/// <summary>Parses a custom name section.</summary>
val parseFromData : data: System.Collections.Immutable.ImmutableArray<byte> -> Result<Lookup, ParseException>

/// <summary>Attempts to retrieve a custom name section from a module's custom sections.</summary>
/// <returns>
/// <c>Some(Ok)</c> if the custom names were retrieved, <c>Some(Error)</c> if the custom name section is malformed, or <c>None</c> if the
/// module does not contain a custom name section.
/// </returns>
val getCustomNames : Validation.ValidModule -> Result<Lookup, ParseException> option
