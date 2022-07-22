[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.TupleCache

open System.Collections.Generic
open System.Collections.Immutable

open Wacil.Compiler
open Wacil.Compiler.Helpers.Collections

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types

type private Template =
    { Type: ITypeDefOrRef; Fields: ImmutableArray<IFieldDescriptor> }

type Instantiation = { Signature: TypeSignature; Fields: ImmutableArray<IFieldDescriptor> }

let create
    (mdle: ModuleDefinition)
    (mscorlib: AssemblyReference)
    (translateValType: Wasm.Format.ValType -> TypeSignature)
    =
    let sizeTemplateLookup =
        let lookup = Dictionary<int, _>()
        let valueTupleName = sprintf "ValueTuple`%i"
        let tupleFieldName = sprintf "Item%i"
        fun count ->
            if count <= 1 then invalidArg (nameof count) "tuple field count must be greater than zero"
            match lookup.TryGetValue count with
            | true, existing -> existing
            | false, _ ->
                if count > 8 then raise(System.NotSupportedException "Returning more than eight values is not yet supported")

                let vtype = ImportHelpers.importType mdle.DefaultImporter mscorlib "System" (valueTupleName count)
                let mutable fields = ArrayBuilder.Create count

                for i in 1..count do
                    let tupleFieldSignature = GenericParameterSignature(GenericParameterType.Type, i - 1) |> FieldSignature
                    fields.Add(ImportHelpers.importField mdle.DefaultImporter tupleFieldSignature (tupleFieldName i) vtype)

                let template = { Type = vtype; Fields = fields.ToImmutableArray() }
                lookup[count] <- template
                template

    let lookup = Dictionary<System.ReadOnlyMemory<Wasm.Format.ValType>, Instantiation>(comparer = LanguagePrimitives.FastGenericEqualityComparer)
    let mutable tupleFieldTypes = ref(ArrayBuilder.Create())
    fun (types: System.ReadOnlyMemory<_>) ->
        match lookup.TryGetValue types with
        | true, existing -> existing
        | false, _ ->
            let template = sizeTemplateLookup types.Length

            tupleFieldTypes.Value.Clear()
            for ty in types.Span do
                tupleFieldTypes.Value.Add(translateValType ty)

            let instantiation = template.Type.MakeGenericInstanceType(tupleFieldTypes.Value.ToArray())
            let info = { Signature = instantiation; Fields = template.Fields }
            lookup[types] <- info
            info
