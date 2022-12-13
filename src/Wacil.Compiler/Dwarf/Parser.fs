module Wacil.Compiler.Dwarf.Parser

type Dwarf = { unused: int }

let fromModuleSections (sections: Wacil.Compiler.Wasm.Format.Module): Dwarf =
    raise(System.NotImplementedException())
