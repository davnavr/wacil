module Wacil.Program

open System
open System.IO

open Argu

open Wacil.Compiler.Emit

type Options =
    | [<Unique; AltCommandLine("-f")>] Framework of TargetFramework
    | Launch_Debugger
    | [<Hidden>] Debug_Disassemble
    | [<Unique; AltCommandLine("-i")>] Module of ``module.wasm``: string
    | [<Unique>] Namespace of string
    | [<Unique; AltCommandLine("-o")>] Out of file: string
    | [<Unique>] Type of OutputType
    //| [<Unique>] Version of Version

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Framework _ -> "specifies the target framework of the assembly"
            | Launch_Debugger -> "launches the debugger used to debug the compiler"
            | Debug_Disassemble -> "disassembles the input WebAssembly file into the WebAssembly Text Format"
            | Module _ ->
                "the WebAssembly file to convert into a CIL file, defaults to searching for a WebAssembly file in the current \
                working directory if omitted"
            | Namespace _ ->
                "the name of the namespace that will contain the class generated from the WebAssembly module"
            | Out _ -> "the path to the generated CIL file"
            | Type _ -> "whether the generated CIL file is an assembly or module, defaults to generating an assembly"

let parser = ArgumentParser.Create<Options>(programName = "wacil")

let getFileArgument (args: ParseResults<Options>) (arg: Quotations.Expr<string -> Options>) defFileThunk =
    (args.TryGetResult(expr = arg): string option)
    |> Option.map FileInfo
    |> Option.defaultWith defFileThunk

[<EntryPoint>]
let main argv =
    try
        let args = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)

        if args.Contains <@ Launch_Debugger @> then System.Diagnostics.Debugger.Launch() |> ignore

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

        Module.compileToStream
            { TargetFramework = args.GetResult(<@ Framework @>, defaultValue = TargetFramework.Net6)
              OutputType = args.GetResult(<@ Type @>, defaultValue = OutputType.Assembly)
              Name = oname
              Version = Version(0, 0, 0, 0)
              RuntimeVersion = Version(1, 0, 0, 0)
              Namespace = "" }
            input''
            writer

        0
    with
    | :? ArguException as ex ->
        stderr.WriteLine ex.Message
        -1
