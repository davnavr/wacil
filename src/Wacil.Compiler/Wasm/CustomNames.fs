module Wacil.Compiler.Wasm.CustomNames

open System.Collections.Generic
open System.Collections.Immutable

open Wacil.Compiler.Helpers

type ParseException = Parser.ParseException

[<NoComparison; NoEquality>]
type FunctionNames =
    { mutable Name: string
      mutable Locals: Dictionary<Format.LocalIdx, string> }

[<Sealed>]
type Lookup
    (
        mdle: string,
        functions: Dictionary<Format.FuncIdx, FunctionNames>
    )
    =
    member _.ModuleName = mdle

    member _.GetFunctionName index =
        match functions.TryGetValue index with
        | true, entry -> entry.Name
        | false, _ -> System.String.Empty

    member _.GetLocalName(parent, index) =
        match functions.TryGetValue parent with
        | true, { Locals = null } | false, _ -> System.String.Empty
        | true, entry ->
            match entry.Locals.TryGetValue index with
            | true, name -> name
            | false, _ -> System.String.Empty

let parseFromData (data: ImmutableArray<byte>) =
    let reader = Parser.Reader(new IO.ReadOnlyMemoryStream(data))
    try
        let mutable mdle = System.String.Empty
        let functions = Dictionary()

        let subsectionTagBuffer = Span.stackalloc 1
        let highestSubsectionTag = ValueSome 0uy
        while reader.Read subsectionTagBuffer > 0 do
            let size = reader.ReadUnsignedInteger() |> Checked.int32
            let subsectionStartOffset = reader.Offset

            let id = subsectionTagBuffer[0]
            if highestSubsectionTag.IsSome && id < highestSubsectionTag.Value then
                failwithf "Name subsection IDs must be in descending order and may only appear once"

            match id with
            | 0uy -> mdle <- reader.ReadName()
            | 1uy ->
                for _ = 1 to Checked.int32(reader.ReadUnsignedInteger()) do
                    let index: Format.FuncIdx = reader.ReadIndex()
                    let entry =
                        match functions.TryGetValue index with
                        | true, ({ Name = "" } as existing) -> existing
                        | true, _ -> failwithf "Name already exists for function #%i" (int32 index)
                        | false, _ ->
                            let entry = { Name = System.String.Empty; Locals = null }
                            functions[index] <- entry
                            entry

                    entry.Name <- reader.ReadName()
            | 2uy ->
                for _ = 1 to Checked.int32(reader.ReadUnsignedInteger()) do
                    let parent: Format.FuncIdx = reader.ReadIndex()
                    let localNameCount = Checked.int32(reader.ReadUnsignedInteger())
                    let locals =
                        match functions.TryGetValue parent with
                        | true, ({ Locals = null } as entry) ->
                            entry.Locals <- Dictionary localNameCount
                            entry.Locals
                        | true, _ -> failwithf "Local names already exist for function #%i" (int32 parent)
                        | false, _ ->
                            let entry = { Name = System.String.Empty; Locals = Dictionary localNameCount }
                            functions[parent] <- entry
                            entry.Locals

                    for _ = 1 to localNameCount do
                        let index: Format.LocalIdx = reader.ReadIndex()
                        if not(locals.TryAdd(index, reader.ReadName())) then
                            failwithf "Local #%i in function #%i already has name" (int32 index) (int32 parent)
            | bad -> failwithf "0x%02X is not a valid name subsection ID" bad

            let actualSubsectionSize = reader.Offset - subsectionStartOffset
            if actualSubsectionSize <> size then
                failwithf "expected %A name subsection to contain 0x%02X bytes, but got 0x%02X bytes" id size actualSubsectionSize

        Ok(Lookup(mdle, functions))
    with
    | ex -> Error(ParseException(reader.Offset, ex))

let getCustomNames (mdle: Validation.ValidModule) =
    let mutable result = None
    let mutable index = 0
    while result.IsNone && index < mdle.CustomSections.Length do
        let section = mdle.CustomSections[index]
        if section.Name = "name" then result <- Some(parseFromData section.Contents)
        index <- index + 1
    result
