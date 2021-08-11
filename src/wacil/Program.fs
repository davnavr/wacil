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
    | [<Unique>] Namespace of string
    | No_Address_Space_Layout_Randomization
    | [<Unique; AltCommandLine("-o")>] Out of file: string
    //| [<Unique>] Skip_Init_Locals
    | [<Unique>] Type of FileType

    interface IArgParserTemplate with
        member this.Usage =
            match this with
            //| Framework _ -> "specifies the target framework of the assembly, defaults to .NET 5"
            | Launch_Debugger -> "launches the debugger used to debug the compiler"
            | Module _ ->
                "the WebAssembly file to convert into a CIL file, defaults to searching for a WebAssembly file in the current \
                working directory if omitted"
            | Namespace _ ->
                "the name of the namespace that will contain the class generated from the WebAssembly module"
            | No_Address_Space_Layout_Randomization -> "Disables ASLR, the C# and F# compilers enable ASLR by default"
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

        let oname = Path.GetFileNameWithoutExtension output.Name

        use writer = output.OpenWrite()

        Generate.toStream
            input'
            { ModuleFileName = oname
              FileType = ttype
              HighEntropyVA = not(args.Contains <@ No_Address_Space_Layout_Randomization @>)
              TargetFramework = ".NETCoreApp,Version=v5.0" // TODO: Make option to allow setting of target framework
              Namespace = args.TryGetResult <@ Namespace @> |> Option.defaultValue String.Empty
              MainClassName = oname }
            writer
        0
    with
    | :? ArguException as ex ->
        stderr.WriteLine ex.Message
        -1
