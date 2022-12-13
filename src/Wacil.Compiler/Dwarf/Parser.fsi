/// <summary>
/// Contains functions for parsing DWARF debugging sections within a WebAssembly module.
/// </summary>
/// <remarks>
/// The format of the debugging information is based on
/// <a href="https://yurydelendik.github.io/webassembly-dwarf/">the DWARF for WebAssembly document.</a>
/// as well as the <a href="https://dwarfstd.org/Download.php">DWARF debugging format standard.</a>.
/// </remarks>
[<RequireQualifiedAccess>]
module Wacil.Compiler.Dwarf.Parser

type Dwarf;

/// Reads DWARF debugging data from the given WebAssembly sections.
val fromModuleSections: sections: Wacil.Compiler.Wasm.Format.Module -> Dwarf;
