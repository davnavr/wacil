[<RequireQualifiedAccess>]
module Wasm.Read

open Wasm.Format

[<NoComparison; NoEquality>]
type State

type State with
    override ToString: unit -> string

[<Class>]
type ReadException =
    inherit System.Exception

    /// The offset from the start of the file of the byte that was being read when the exception was thrown.
    member Offset: uint32

    member State: State

val inline (|ReadException|) : e: ReadException -> struct(uint32 * State * exn)

// TODO: Have all exception types in Format and Read modules be in a separate Validation module.

// TODO: How to say that ReadException should be caught, not these specific exceptions.
exception InvalidMagicException of actual: byte[]
exception InvalidVersionException of actual: byte[]
exception InvalidSectionIdException of id: SectionId * index: int32

type InvalidMagicException with override Message: string
type InvalidVersionException with override Message: string
type InvalidSectionIdException with override Message: string

/// <exception cref="T:System.ArgumentNullException">
/// Thrown when the <paramref name="stream"/> is <see langword="null"/>.
/// </exception>
/// <exception cref="T:System.ArgumentException">Thrown when the <paramref name="stream"/> does not support reading.</exception>
/// <exception cref="T:Wasm.Read.ReadException" />
val fromStream : stream: System.IO.Stream -> ValidatedModule

/// <exception cref="T:System.IO.FileNotFoundException">
/// Thrown when the file specified by the <paramref name="path"/> does not exist.
/// </exception>
val fromPath : path: string -> ValidatedModule
