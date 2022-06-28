#r "./out/Wacil.Runtime.dll"
#r "./out/hello_world.dll"

let mutable instance: hello__world = null

let console_log(address: int32) =
    printfn "0x%08X" address

let abort (_: int32) (_: int32) (_: int32) (_: int32) =
    failwith "ABORTED"

instance <- hello__world(hello__world.env(hello__world.env.console_log(console_log), hello__world.env.abort(abort)))
instance.hello()
