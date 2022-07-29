[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.TupleCache

open System.Collections.Generic
open System.Collections.Immutable

open Wacil.Compiler
open Wacil.Compiler.Helpers
open Wacil.Compiler.Helpers.Collections

open AsmResolver.PE.DotNet.Metadata.Tables.Rows

open AsmResolver.DotNet.Code.Cil
open AsmResolver.PE.DotNet.Cil

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types

type private Template =
    { Type: ITypeDefOrRef
      FieldNames: string[]
      FieldTypes: TypeSignature[] }

type Instantiation =
    { Signature: TypeSignature
      ReverseConstructor: IMethodDefOrRef
      Fields: ImmutableArray<IFieldDescriptor> }

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
            if count <= 1 then invalidArg (nameof count) "tuple field count must be greater than one"
            match lookup.TryGetValue count with
            | true, existing -> existing
            | false, _ ->
                if count > 8 then raise(System.NotSupportedException "Returning more than eight values is not yet supported")

                let vtype = ImportHelpers.importType mdle.DefaultImporter mscorlib "System" (valueTupleName count)

                let mutable tupleFieldNames = ArrayBuilder<string>.Create count
                let mutable tupleFieldTypes = ArrayBuilder<TypeSignature>.Create count

                for i in 1..count do
                    tupleFieldNames.Add(tupleFieldName i)
                    tupleFieldTypes.Add(GenericParameterSignature(GenericParameterType.Type, i - 1))

                let template =
                    { Type = vtype
                      FieldNames = tupleFieldNames.ToArray()
                      FieldTypes = tupleFieldTypes.ToArray() }

                lookup[count] <- template
                template

    let lookup = Dictionary<ImmutableArray<_>, Instantiation>(comparer = LanguagePrimitives.FastGenericEqualityComparer)
    let mutable tupleFieldTypes = ref(ArrayBuilder.Create())
    fun types ->
        match lookup.TryGetValue types with
        | true, existing -> existing
        | false, _ ->
            let template = sizeTemplateLookup types.Length

            let actualFieldTypes =
                tupleFieldTypes.contents.Clear()
                for ty in types do tupleFieldTypes.contents.Add(translateValType ty)
                tupleFieldTypes.contents.ToArray()

            let instantiation = template.Type.MakeGenericInstanceType actualFieldTypes
            let specification = TypeSpecification instantiation

            let actualTupleConstructor =
                ImportHelpers.importConstructor mdle template.FieldTypes specification

            let mutable actualTupleFields = Array.zeroCreate template.FieldTypes.Length
            for i = 0 to template.FieldTypes.Length - 1 do
                actualTupleFields[i] <-
                    ImportHelpers.importField
                        mdle.DefaultImporter
                        (FieldSignature template.FieldTypes[i])
                        template.FieldNames[i]
                        specification

            let reverseConstructorHelper =
                DefinitionHelpers.addMethodDefinition
                    (mdle.GetModuleType())
                    (MethodSignature(CallingConventionAttributes.Default, instantiation, Array.rev actualFieldTypes))
                    MethodAttributes.Static
                    "ReverseConstructor"

            reverseConstructorHelper.ImplAttributes <- CilHelpers.methodImplAggressiveInlining

            for i = 1 to template.FieldNames.Length do
                reverseConstructorHelper.ParameterDefinitions.Add(ParameterDefinition(
                    uint16 i,
                    template.FieldNames[i - 1],
                    Unchecked.defaultof<_>
                ))

            do
                reverseConstructorHelper.CilMethodBody <- CilMethodBody reverseConstructorHelper
                let il = reverseConstructorHelper.CilMethodBody.Instructions
                for i = types.Length - 1 downto 0 do il.Add(CilInstruction.CreateLdarg(uint16 i))
                il.Add(CilInstruction(CilOpCodes.Newobj, actualTupleConstructor))
                il.Add(CilInstruction CilOpCodes.Ret)

            let info =
                { Signature = instantiation
                  ReverseConstructor = reverseConstructorHelper
                  Fields = Unsafe.Array.toImmutable actualTupleFields }

            lookup[types] <- info
            info
