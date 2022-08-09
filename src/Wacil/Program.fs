module Wacil.Program

open System
open System.IO

open Argu

open Wacil.Compiler.Emit

[<RequireQualifiedAccess>]
type DefinedMemoryImplementation =
    | Array
    | Segmented

    member this.ToImplementation() =
        match this with
        | Array -> MemoryImplementation.Array
        | Segmented -> MemoryImplementation.Segmented

type Options =
    | [<Unique; AltCommandLine("-i")>] Module of ``module.wasm``: string
    | [<Unique>] Namespace of string
    | [<Unique; AltCommandLine("-o")>] Out of file: string
    //| [<Unique>] Type of OutputType
    //| [<Unique>] Version of Version
    | [<Unique>] Defined_Memory_Implementation of DefinedMemoryImplementation
    | [<Hidden>] Debug_Disassemble
    | [<Unique>] Diagnostic_Output_Path of file: string
    | [<Unique; AltCommandLine("-f")>] Framework of TargetFramework
    | Omit_Custom_Names
    | Launch_Debugger

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Module _ ->
                "The WebAssembly file to convert into a CIL file, defaults to searching for a WebAssembly file in the current working \
                directory if omitted"
            | Namespace _ -> "The name of the namespace that will contain the class generated from the WebAssembly module"
            | Out _ -> "The path to the generated CIL file"
            //| Type _ -> "Whether the generated CIL file is an assembly or module, defaults to generating an assembly"
            | Defined_Memory_Implementation _ -> "The runtime class to use when translating defined WebAssembly memories"
            | Debug_Disassemble -> "Disassembles the input WebAssembly file into the WebAssembly Text Format"
            | Diagnostic_Output_Path _ ->
                "Specifies a path to a file where diagnostic information should be written to, defaults to standard error"
            | Omit_Custom_Names -> "If specified, skips parsing the custom name section of the WebAssembly module"
            | Framework _ -> "Specifies the target framework of the assembly"
            | Launch_Debugger -> "Launches the debugger used to debug the compiler"

let parser = ArgumentParser.Create<Options>(programName = "wacil")

let getFileArgument (args: ParseResults<Options>) (arg: Quotations.Expr<string -> Options>) defFileThunk =
    (args.TryGetResult(expr = arg): string option)
    |> Option.map FileInfo
    |> Option.defaultWith defFileThunk

[<EntryPoint>]
let main argv =
    try
        let args = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

        if args.Contains <@ Launch_Debugger @> then Diagnostics.Debugger.Launch() |> ignore

        let input =
            getFileArgument args <@ Module @> <| fun() -> 
                let files =
                    Directory.GetFiles Environment.CurrentDirectory
                    |> Seq.where (fun path -> Path.GetExtension path = ".wasm")
                    |> Seq.map FileInfo

                match Seq.tryHead files with
                | Some file' -> file'
                | None -> FileNotFoundException("No WebAssembly files were found in the current directory") |> raise

        let input' =
            use reader = input.OpenRead()
            Compiler.Wasm.Parser.parseFromStream reader

        let input'' = Compiler.Wasm.Validation.Validate.fromModuleSections input'
        
        if args.Contains <@ Debug_Disassemble @> then
            Compiler.Wasm.Disassemble.disassembleToWriter input'' Console.Out

        let output =
            getFileArgument args <@ Out @> <| fun() -> FileInfo(Path.ChangeExtension(input.FullName, ".dll"))

        let oname = Path.GetFileNameWithoutExtension output.Name

        use writer = output.OpenWrite()

        let options = Options(oname)

        match args.TryGetResult <@ Framework @> with
        | Some framework -> options.TargetFramework <- framework
        | None -> ()

        match args.TryGetResult <@ Defined_Memory_Implementation @> with
        | Some implementation -> options.MemoryDefinitionImplementation <- implementation.ToImplementation()
        | None -> ()

        options.CustomNames <-
            if args.Contains <@ Omit_Custom_Names @>
            then None
            else
                match Compiler.Wasm.CustomNames.getCustomNames input'' with
                | Some(Ok names) -> Some names
                | Some(Error e) ->
                    eprintfn "Invalid custom name section: %A" e
                    None
                | None -> None

        let disposeDiagnosticStream, diagnosticOutputStream =
            match args.TryGetResult <@ Diagnostic_Output_Path @> with
            | Some path ->
                Directory.CreateDirectory(Path.GetDirectoryName path) |> ignore
                true, new StreamWriter(path) :> TextWriter
            | None ->
                false, stderr

        try
            Diagnostics.handleInvalidCil (fun() -> Module.compileToStream options input'' writer) diagnosticOutputStream
        finally
            if disposeDiagnosticStream then diagnosticOutputStream.Close()

        0
    with
    | :? ArguException as ex ->
        stderr.WriteLine ex.Message
        -1
