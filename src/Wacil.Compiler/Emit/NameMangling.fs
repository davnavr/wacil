/// Helper module used to mangle the names of generated .NET types and members.
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.NameMangling

let private isValidCharacter c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c = '_'

let private isManglingRequired (input: string) =
    if input.Length = 0 then invalidArg (nameof input) "cannot mangle empty name string"
    let mutable manglingIsRequired = false
    let mutable index = 0
    while index < input.Length && not manglingIsRequired do
        let c = input[index]
        if not(isValidCharacter c || (index > 0 && c >= '0' && c <= '9')) then
            manglingIsRequired <- true
        index <- index + 1
    manglingIsRequired

let mangle (buffer: System.Text.StringBuilder) (name: string) =
    if isManglingRequired name then
        buffer.Clear().Append("M_") |> ignore
        for c in name do
            if isValidCharacter c then
                buffer.Append c |> ignore
            else
                Printf.bprintf buffer "_U%04X" (int c)
        buffer.ToString()
    else
        name
