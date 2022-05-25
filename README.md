# wacil
```F#
WebAssembly |> dotnet exec
```

Yet another experimental compiler that translates WebAssembly files (`.wasm`) to .NET assemblies/modules (`.dll`/`.netmodule`).

Currently undergoing a rewrite to clean up the API of the helper library used to parse WebAssembly modules and to replace usage of my incomplete [`FSharpIL`](https://github.com/davnavr/FSharpIL) library with [`System.Reflection.Metadata`](https://www.nuget.org/packages/System.Reflection.Metadata).
