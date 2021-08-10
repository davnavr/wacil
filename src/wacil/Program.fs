module wacil.Program

open System
open System.IO

open Argu

open Wasm
open Wasm.Format

open wacil.Generator

type Options =
    //| [<Unique; AltCommandLine("-f")>] Framework of TargetFramework
    | Launch_Debugger
    | [<Unique>] Module of ``module.wasm``: string
    | Name of string
    //| [<Unique>] Namespace of string
    | No_Address_Space_Layout_Randomization
    | [<Unique; AltCommandLine("-o")>] Out of file: string
    | [<Unique>] Type of FileType

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Launch_Debugger -> "Launches the debugger used to debug the compiler"
            | Module _ ->
                "Specifies the WebAssembly file to convert into a CIL file, defaults to searching for a WebAssembly file in the \
                current working directory if omitted"
            | Name _ ->
                "Specifies the name of the generated assembly and/or module, defaults to the file name of the WebAssembly \
                module minus the extension"
            | No_Address_Space_Layout_Randomization -> "Disables ASLR, the C# and F# compilers enable ASLR by default"
            | Out _ -> "Specifies the path to the generated CIL file"
            | Type _ -> "Specifies whether the generated CIL file is an assembly or module, defaults to generating an assembly"

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

        let ttype = args.TryGetResult <@ Type @> |> Option.defaultValue Assembly

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
            ReadModule.fromStream reader

        let output =
            getFileArgument args <@ Out @> <| fun() ->
                FileInfo(Path.ChangeExtension(input.FullName, FileType.extension ttype))

        use writer = output.OpenWrite()

        Generate.toStream
            input'
            { ModuleFileName =
                args.TryGetResult <@ Name @>
                |> Option.defaultWith (fun() -> Path.GetFileNameWithoutExtension output.Name)
              FileType = ttype
              HighEntropyVA = not(args.Contains <@ No_Address_Space_Layout_Randomization @>) }
            writer
        0
    with
    | :? ArguException as ex ->
        stderr.WriteLine ex.Message
        -1
