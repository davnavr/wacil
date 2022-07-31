// Runs F# example scripts

open System.Diagnostics
open System.IO

do
    for scriptFilePath in Directory.EnumerateFiles(__SOURCE_DIRECTORY__, "test.fsx", SearchOption.AllDirectories) do
        use scriptRunProcess =
            let options = ProcessStartInfo "dotnet"
            options.ArgumentList.Add "fsi"
            options.ArgumentList.Add scriptFilePath
            Process.Start options

        scriptRunProcess.WaitForExit()

        if scriptRunProcess.ExitCode <> 0 then
            failwithf "%s exited with code %i" scriptFilePath scriptRunProcess.ExitCode
