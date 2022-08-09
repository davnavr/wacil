/// <summary>Contains functions for parsing the contents of a WebAssembly module.</summary>
/// <remarks>The parser functions only check that modules are syntactically correct.</remarks>
[<RequireQualifiedAccess>]
module Wacil.Compiler.Wasm.Parser

open System.Collections.Immutable

[<Sealed; Class>]
type internal Reader =
    new : source: System.IO.Stream -> Reader

    member ReadByte : unit -> byte
    member Read : buffer: System.Span<byte> -> int
    member ReadAll : buffer: System.Span<byte> -> unit
    member ReadUnsignedInteger : unit -> uint64
    member inline ReadIndex : unit -> ^T when ^T : (static member From: uint64 -> 'T)
    member ReadSignedInteger : unit -> int64
    member ReadSignedIntegerOrNegativeZero : unit -> int64 voption
    member ReadFloat32 : unit -> single
    member ReadFloat64 : unit -> double
    member ReadName : unit -> Format.Name
    member Offset : int

[<Sealed; Class>]
type InvalidMagicException =
    inherit System.Exception

    member Magic : ImmutableArray<byte>

[<Sealed; Class>]
type ParseException =
    inherit System.Exception

    new : offset: int * inner: exn -> ParseException

    member Offset : int

/// <summary>Parses a WebAssembly module from a <see cref="T:System.IO.Stream"/>.</summary>
/// <exception cref="T:System.ArgumentException">Thrown when the <paramref name="stream"/> does not support reading.</exception>
val parseFromStream: stream: System.IO.Stream -> Format.Module

/// <summary>Parses the WebAssembly module file at the specified <paramref name="path"/>.</summary>
/// <exception cref="T:System.IO.FileNotFoundException">Thrown when the file specified by the <paramref name="path"/> does not exist.</exception>
val parseFromPath: path: string -> Format.Module
