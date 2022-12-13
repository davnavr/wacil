/// <summary>Contains functions for parsing DWARF debugging sections within a WebAssembly module.</summary>
[<RequireQualifiedAccess>]
module Wacil.Compiler.Dwarf.Parser

type Dwarf;

/// Reads DWARF debugging data from the given WebAssembly sections.
val fromModuleSections: sections: Wacil.Compiler.Wasm.Format.Module -> Dwarf;
