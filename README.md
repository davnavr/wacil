# wacil
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
```F#
"module.wasm" |> dotnet exec
```

Yet another experimental compiler that translates WebAssembly files (`.wasm`) to .NET assemblies/modules (`.dll`/`.netmodule`).

Powered by [`AsmResolver`](https://github.com/Washi1337/AsmResolver/), a high level library for generating .NET metadata.

Currently, work is focused on supporting most of the features from [version 1.0 of the WebAssembly specification](https://www.w3.org/TR/wasm-core-1/), as well as some features in [the version 2.0 draft](https://www.w3.org/TR/wasm-core-2/).

# How does it work?
Like other "compilers" from WebAssembly to CIL, WebAssembly instructions are translated to CIL instructions, which is more efficient that simply writing a WASM interpreter in C# or F#. Unlike other compilers however, all modules translated by Wacil include a reference to a runtime library `Wacil.Runtime.dll`. Having a separate runtime library rather than simply embedding code in the generated assembly has its own drawbacks, but has the advantage of avoiding code duplication.
