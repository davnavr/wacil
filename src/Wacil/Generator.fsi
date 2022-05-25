module wacil.Generator

open FSharpIL.PortableExecutable

open Wasm.Format

[<NoComparison; NoEquality>]
type FileType =
    | Assembly
    | Netmodule

[<RequireQualifiedAccess>]
module FileType =
    val extension : FileType -> string

[<NoComparison; NoEquality>]
type Options =
    { ModuleFileName: string
      FileType: FileType
      HighEntropyVA: bool
      TargetFramework: string
      MainClassName: string }

[<RequireQualifiedAccess>]
module Generate =
    val toPE : ``module``: ValidatedModule -> options: Options -> PEFile
