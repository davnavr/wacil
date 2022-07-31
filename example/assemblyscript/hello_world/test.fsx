#r "./out/Wacil.Runtime.dll"
#r "./out/hello_world.dll"

let mutable instance: hello_world.hello_world = null

let console_log(address: int32) =
    // Layout of string objects is documented here:
    // https://www.assemblyscript.org/runtime.html#memory-layout
    let stringByteLength = (instance.memory :> Wacil.Runtime.IMemory32).ReadInt32(address - 4, 2uy)

    let mutable offset = 0
    let contents = System.Text.StringBuilder(stringByteLength / 2)
    while offset < stringByteLength do
        let value = (instance.memory :> Wacil.Runtime.IMemory32).ReadInt16(address + offset, 1uy)
        contents.Append(Checked.char(uint16 value)) |> ignore
        offset <- Checked.(+) offset 2
    System.Console.WriteLine(contents.ToString())

let abort (_: int32) (_: int32) (_: int32) (_: int32) =
    failwith "ABORTED"

instance <- hello_world.hello_world(hello_world.env(console_log, abort))
instance.hello()
