[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.DelegateCache

open System.Collections.Immutable
open System.Collections.Generic

open Wacil.Compiler.Helpers.Collections

open AsmResolver.PE.DotNet.Metadata.Tables.Rows;

open AsmResolver.DotNet.Code.Cil
open AsmResolver.PE.DotNet.Cil

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types

let private signatureContainsByRefParameters (signature: MethodSignature) =
    let mutable hasByRef = TypeHelpers.isByRef signature.ReturnType

    // Code generator usually puts byref parameters at the end, so start search there
    let mutable index = 0
    while not hasByRef && index < signature.ParameterTypes.Count do
        hasByRef <- TypeHelpers.isByRef signature.ParameterTypes[index]
        index <- index + 1

    hasByRef

let private signatureHasPredefinedDelegate (signature: MethodSignature) =
    not (signatureContainsByRefParameters signature) && signature.ParameterTypes.Count <= 16

type private Template = { Type: ITypeDefOrRef; Invoke: IMethodDefOrRef }

type Instantiation =
    { TypeSignature: TypeSignature
      FieldSignature: FieldSignature
      Invoke: IMethodDefOrRef
      InvokeHelper: MethodDefinition
      ConvertHelper: MethodDefinition }

/// <summary>
/// Creates a factory for delegate types.
/// </summary>
/// <remarks>
/// Attempts to use standard library delegate types where possible, such as <see cref="T:System.Action`1"/> or
/// <see cref="T:System.Func`1"/>. In some cases (e.g. when <see langword="ref"/> parameters are present), a new delegate type is
/// generated.
/// </remarks>
let create (mdle: ModuleDefinition) (mscorlib: AssemblyReference) (syslib: SystemLibrary.References) =
    let systemDelegateCache =
        let lookup = Dictionary<struct(int * bool), Template>()
        let funcTypeName = sprintf "Func`%i"
        let actionTypeName = sprintf "Action`%i"
        let templateParameterTypes = ImmutableArray.CreateBuilder()
        fun (signature: MethodSignature) ->
            let key = struct(signature.ParameterTypes.Count, signature.ReturnsValue)
            match lookup.TryGetValue key with
            | true, existing -> existing
            | false, _ ->
                let delegateTypeReference =
                    if signature.ReturnsValue
                    then funcTypeName(signature.ParameterTypes.Count + 1)
                    elif signature.ParameterTypes.Count = 0 then "Action"
                    else actionTypeName(signature.ParameterTypes.Count)
                    |> ImportHelpers.importType mdle.DefaultImporter mscorlib "System"

                templateParameterTypes.Clear()
                for i in 0..signature.ParameterTypes.Count - 1 do
                    templateParameterTypes.Add(GenericParameterSignature(GenericParameterType.Type, i) :> TypeSignature)

                let templateInvokeSignature =
                    let templateReturnType =
                        if signature.ReturnsValue
                        then GenericParameterSignature(GenericParameterType.Type, signature.ParameterTypes.Count) :> TypeSignature
                        else mdle.CorLibTypeFactory.Void

                    MethodSignature(
                        CallingConventionAttributes.HasThis,
                        templateReturnType,
                        templateParameterTypes.ToArray()
                    )

                let delegateInvokeMethod =
                    delegateTypeReference.CreateMemberReference(
                        "Invoke",
                        templateInvokeSignature
                    )
                    |> mdle.DefaultImporter.ImportMethod

                let template = { Type = delegateTypeReference; Invoke = delegateInvokeMethod }
                lookup[key] <- template
                template

    fun (signature: MethodSignature) (*generatedDelegateName*) ->
        if signature.Attributes <> CallingConventionAttributes.HasThis then
            invalidArg (nameof signature) (sprintf "Attempt to generate delegate type with %A" signature.Attributes)
            
        if signatureHasPredefinedDelegate signature then
            let template = systemDelegateCache signature
            let originalParameterCount = signature.ParameterTypes.Count
            
            let instantiation =
                let mutable genericTemplateArguments = ArrayBuilder.Create(originalParameterCount + 1)
                for ty in signature.ParameterTypes do genericTemplateArguments.Add ty
                if signature.ReturnsValue then genericTemplateArguments.Add signature.ReturnType
                template.Type.MakeGenericInstanceType(genericTemplateArguments.ToArray())

            let specification = TypeSpecification instantiation

            let invoke =
                specification.CreateMemberReference(
                    template.Invoke.Name,
                    template.Invoke.Signature
                )
                |> mdle.DefaultImporter.ImportMethodOrNull

            let staticInvocationHelper =
                let invokeHelperSignature =
                    let mutable ptypes = ArrayBuilder.Create(originalParameterCount + 1)
                    for ty in signature.ParameterTypes do ptypes.Add ty
                    ptypes.Add instantiation
                    MethodSignature(CallingConventionAttributes.Default, signature.ReturnType, ptypes.ToArray())

                let definition =
                    DefinitionHelpers.addMethodDefinition
                        (mdle.GetModuleType())
                        invokeHelperSignature
                        MethodAttributes.Static
                        "Invoke"

                definition.ImplAttributes <- CilHelpers.methodImplAggressiveInlining
                definition.CilMethodBody <- CilMethodBody definition
                let il = definition.CilMethodBody.Instructions
                il.Add(CilInstruction.CreateLdarg(uint16 originalParameterCount))
                for i = 0 to originalParameterCount - 1 do il.Add(CilInstruction.CreateLdarg(uint16 i))
                il.Add(CilInstruction(CilOpCodes.Callvirt, invoke))
                il.Add(CilInstruction CilOpCodes.Ret)
                definition

            let staticConversionHelper =
                let definition =
                    let signature =
                        MethodSignature(
                            CallingConventionAttributes.Default,
                            instantiation,
                            [| syslib.MulticastDelegate.Signature |]
                        )

                    DefinitionHelpers.addMethodDefinition
                        (mdle.GetModuleType())
                        signature
                        MethodAttributes.Static
                        "Convert"

                definition.CilMethodBody <- CilMethodBody definition
                let il = definition.CilMethodBody.Instructions
                let convertDelegateBranch = CilInstruction CilOpCodes.Pop
                il.Add(CilInstruction CilOpCodes.Ldarg_0)
                il.Add(CilInstruction(CilOpCodes.Isinst, specification :> ITypeDefOrRef))
                il.Add(CilInstruction(CilOpCodes.Brfalse_S, convertDelegateBranch))

                // Delegate is already of the correct type
                il.Add(CilInstruction CilOpCodes.Ret)

                // A new delegate needs to be created
                il.Add convertDelegateBranch
                il.Add(CilInstruction(CilOpCodes.Ldtoken, specification :> ITypeDefOrRef))
                il.Add(CilInstruction(CilOpCodes.Call, syslib.Type.GetTypeFromHandle))
                il.Add(CilInstruction CilOpCodes.Ldarg_0)
                il.Add(CilInstruction(CilOpCodes.Call, syslib.Delegate.GetTarget))
                il.Add(CilInstruction CilOpCodes.Ldarg_0)
                il.Add(CilInstruction(CilOpCodes.Call, syslib.Delegate.GetMethod))
                il.Add(CilInstruction(CilOpCodes.Call, syslib.Delegate.CreateDelegate))
                il.Add(CilInstruction CilOpCodes.Ret)
                definition

            // TODO: Maybe cache the instantiations?
            { TypeSignature = instantiation
              FieldSignature = FieldSignature instantiation
              Invoke = invoke
              InvokeHelper = staticInvocationHelper
              ConvertHelper = staticConversionHelper }
        else
            failwith "TODO: generatedDelegateCache"
