/// <summary>Contains functions for parsing the contents of a WebAssembly module.</summary>
/// <remarks>The parser functions only check that modules are syntactically correct.</remarks>
[<RequireQualifiedAccess>]
module Wacil.Compiler.Wasm.Parser

/// <summary>Parses a WebAssembly module from a <see cref="T:System.IO.Stream"/>.</summary>
/// <exception cref="T:System.ArgumentException">Thrown when the <paramref name="stream"/> does not support reading.</exception>
val parseFromStream: stream: System.IO.Stream -> Format.Module

/// <summary>Parses the WebAssembly module file at the specified <paramref name="path"/>.</summary>
/// <exception cref="T:System.IO.FileNotFoundException">Thrown when the file specified by the <paramref name="path"/> does not exist.</exception>
val parseFromPath: path: string -> Format.Module
