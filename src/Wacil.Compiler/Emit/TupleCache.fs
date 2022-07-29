[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.TupleCache

open System.Collections.Generic
open System.Collections.Immutable

open Wacil.Compiler
open Wacil.Compiler.Helpers.Collections

open AsmResolver.PE.DotNet.Metadata.Tables.Rows

open AsmResolver.DotNet.Code.Cil
open AsmResolver.PE.DotNet.Cil

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types

type private Template =
    { Type: ITypeDefOrRef
      Constructor: IMethodDefOrRef
      Fields: ImmutableArray<IFieldDescriptor> }

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

                let mutable fields = ArrayBuilder<IFieldDescriptor>.Create count
                let mutable constructorParameterTypes = ArrayBuilder<TypeSignature>.Create count

                for i in 1..count do
                    let tupleFieldType = GenericParameterSignature(GenericParameterType.Type, i - 1)
                    fields.Add(ImportHelpers.importField mdle.DefaultImporter (FieldSignature tupleFieldType) (tupleFieldName i) vtype)
                    constructorParameterTypes.Add tupleFieldType

                let template =
                    { Type = vtype
                      Constructor = ImportHelpers.importConstructor mdle (constructorParameterTypes.ToArray()) vtype
                      Fields = fields.ToImmutableArray() }

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

            let reverseConstructorHelper =
                DefinitionHelpers.addMethodDefinition
                    (mdle.GetModuleType())
                    (MethodSignature(CallingConventionAttributes.Default, instantiation, actualFieldTypes))
                    MethodAttributes.Static
                    "ReverseConstructor"

            do
                reverseConstructorHelper.CilMethodBody <- CilMethodBody reverseConstructorHelper
                let il = reverseConstructorHelper.CilMethodBody.Instructions
                for i = types.Length - 1 downto 0 do il.Add(CilInstruction.CreateLdarg(uint16 i))
                il.Add(CilInstruction(CilOpCodes.Newobj, template.Constructor))
                il.Add(CilInstruction CilOpCodes.Ret)

            let info = { Signature = instantiation; ReverseConstructor = reverseConstructorHelper; Fields = template.Fields }
            lookup[types] <- info
            info
