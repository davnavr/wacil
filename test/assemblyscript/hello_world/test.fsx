#r "./out/Wacil.Runtime.dll"
#r "./out/hello_world.dll"

let mutable instance: hello__world = null

let console_log(address: int32) =
    let address = uint32 address

    // Layout of string objects is documented here:
    // https://www.assemblyscript.org/runtime.html#memory-layout
    let lengthFieldAddress = address - 4u
    let stringByteLength = uint32(Wacil.Runtime.Memory.ReadInt32(address - 4u, instance.memory, 0u, 2uy))

    let mutable offset = 0u
    let contents = System.Text.StringBuilder(Checked.int32 stringByteLength / 2)
    while offset < stringByteLength do
        let value = Wacil.Runtime.Memory.ReadInt16(address + offset, instance.memory, 0u, 1uy)
        contents.Append(Checked.char(uint16 value)) |> ignore
        offset <- Checked.(+) offset 2u
    System.Console.WriteLine(contents.ToString())

let abort (_: int32) (_: int32) (_: int32) (_: int32) =
    failwith "ABORTED"

instance <- hello__world(hello__world.env(hello__world.env.console_log(console_log), hello__world.env.abort(abort)))
instance.hello()
