/// Helper module for translation of WebAssembly module memories.
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.MemoryTranslator

open Wacil.Compiler
open Wacil.Compiler.Helpers.Collections

open AsmResolver.DotNet
open AsmResolver.DotNet.Code.Cil

let translateModuleMemories
    (rtlib: RuntimeLibrary.References)
    (moduleClassDefinition: TypeDefinition)
    (wasm: Wasm.Validation.ValidModule)
    =
    let mutable fields = ArrayBuilder<FieldDefinition>.Create(wasm.Imports.Imports.Memories.Length + wasm.Memories.Length)

    let memoryInitializationCode (body: CilMethodBody) =
        failwith "TODO: Append memory initialization code to the constructor"

    failwith "TODO: Translate memories"

    (fields.ToImmutableArray(), memoryInitializationCode)
