module Wacil.Compiler.Emit.Module

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open Microsoft.FSharp.Core.Printf

open AsmResolver
open AsmResolver.PE.DotNet.Metadata.Tables.Rows;

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures
open AsmResolver.DotNet.Signatures.Types

open AsmResolver.PE.DotNet.Cil
open AsmResolver.DotNet.Code.Cil

open Wacil.Compiler.Helpers
open Wacil.Compiler.Helpers.Collections

open Wacil.Compiler.Wasm.Format
open Wacil.Compiler.Wasm.Validation

type CilInstruction with
    static member CreateLdloc index =
        match index with
        | 0 -> CilInstruction CilOpCodes.Ldloc_0
        | 1 -> CilInstruction CilOpCodes.Ldloc_1
        | 2 -> CilInstruction CilOpCodes.Ldloc_2
        | 3 -> CilInstruction CilOpCodes.Ldloc_3
        | _ when index <= 255 -> CilInstruction(CilOpCodes.Ldloc_S, index)
        | _ -> CilInstruction(CilOpCodes.Ldloc, index)

    static member CreateLdarg index =
        match index with
        | 0 -> CilInstruction CilOpCodes.Ldarg_0
        | 1 -> CilInstruction CilOpCodes.Ldarg_1
        | 2 -> CilInstruction CilOpCodes.Ldarg_2
        | 3 -> CilInstruction CilOpCodes.Ldarg_3
        | _ when index <= 255 -> CilInstruction(CilOpCodes.Ldarg_S, index)
        | _ -> CilInstruction(CilOpCodes.Ldarg, index)

    static member CreateStloc index =
        match index with
        | 0 -> CilInstruction CilOpCodes.Stloc_0
        | 1 -> CilInstruction CilOpCodes.Stloc_1
        | 2 -> CilInstruction CilOpCodes.Stloc_2
        | 3 -> CilInstruction CilOpCodes.Stloc_3
        | _ when index <= 255 -> CilInstruction(CilOpCodes.Stloc_S, index)
        | _ -> CilInstruction(CilOpCodes.Stloc, index)

    static member CreateStarg index =
        match index with
        | _ when index <= 255 -> CilInstruction(CilOpCodes.Starg_S, index)
        | _ -> CilInstruction(CilOpCodes.Starg, index)

[<NoComparison; NoEquality>]
type RuntimeLibraryTable =
    { Specification: GenericInstanceTypeSignature
      //Constructor: IMethodDefOrRef
      }

/// <summary>Represents the references to the Wacil runtime library (<c>Wacil.Runtime.dll</c>).</summary>
[<NoComparison; NoEquality>]
type RuntimeLibraryReference =
    { Memory: ITypeDefOrRef
      MemorySignature: TypeSignature
      MemoryConstructor: IMethodDefOrRef
      MemoryI32Load: IMethodDefOrRef
      MemoryI32Store: IMethodDefOrRef
      MemoryGrow: IMethodDefOrRef
      Table: ITypeDefOrRef
      InstantiatedTable: RefType -> RuntimeLibraryTable }

[<IsReadOnly; Struct; NoComparison; StructuralEquality>]
type LocalIndex =
    | Arg of a: int32
    | Loc of int32

[<NoComparison; NoEquality>]
type InstructionBlockBuilder =
    { mutable Instructions: System.ReadOnlyMemory<ValidInstruction>
      Labels: IntroducedLabelLookup }

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type TranslatedGlobal =
    { Field: FieldDefinition
      Initializer: MethodDefinition
      Setter: MethodDefinition voption }

[<RequireQualifiedAccess; NoComparison; NoEquality>]
type TranslatedTable =
    { ElementType: RefType
      Field: FieldDefinition }

[<NoComparison; NoEquality>]
type TranslatedFunctionImport =
    { Delegate: TypeDefinition
      Field: FieldDefinition
      Invoke: MethodDefinition }

[<NoComparison; NoEquality>]
type ModuleImport =
    { Class: TypeDefinition
      Signature: TypeDefOrRefSignature
      Field: FieldDefinition
      FunctionLookup: SortedList<string, TranslatedFunctionImport> }

let methodImplAggressiveInlining: MethodImplAttributes = LanguagePrimitives.EnumOfValue 256us

let compileToModuleDefinition (options: Options) (input: ValidModule) =
    let coreLibraryReference =
        match options.TargetFramework with
        | TargetFramework.Net6 -> KnownCorLibs.SystemRuntime_v6_0_0_0

    let stringBuffer = System.Text.StringBuilder()
    let outputName = String.defaultValue "module" options.Name
    let moduleDefinition = new ModuleDefinition(outputName + ".dll", coreLibraryReference)

    let coreSystemObject =
        coreLibraryReference.CreateTypeReference("System", "Object") |> moduleDefinition.DefaultImporter.ImportTypeOrNull
    
    let coreSystemObjectSignature = TypeDefOrRefSignature coreSystemObject

    let coreSystemObjectConstructor =
        coreSystemObject.CreateMemberReference(
            ".ctor",
            new MethodSignature(
                CallingConventionAttributes.HasThis,
                moduleDefinition.CorLibTypeFactory.Void,
                Array.empty
            )
        )
        |> moduleDefinition.DefaultImporter.ImportMethod

    let coreSystemArgumentNullException =
        coreLibraryReference.CreateTypeReference("System", "ArgumentNullException")
        |> moduleDefinition.DefaultImporter.ImportTypeOrNull

    let coreSystemArgumentNullExceptionConstructor =
        coreSystemArgumentNullException.CreateMemberReference(
            ".ctor",
            new MethodSignature(
                CallingConventionAttributes.HasThis,
                moduleDefinition.CorLibTypeFactory.Void,
                Array.singleton moduleDefinition.CorLibTypeFactory.String
            )
        )
        |> moduleDefinition.DefaultImporter.ImportMethod

    // The MulticastDelegate base class is imported here to serve as the base class for any delegate classes generated later
    let coreSystemMulticastDelegate =
        coreLibraryReference.CreateTypeReference("System", "MulticastDelegate")
        |> moduleDefinition.DefaultImporter.ImportTypeOrNull

    let coreSystemMulticastDelegateSignature = TypeDefOrRefSignature coreSystemMulticastDelegate

    let assemblyDefinition =
        match options.OutputType with
        | OutputType.Assembly ->
            let assembly = new AssemblyDefinition(outputName, options.Version)
            assembly.Modules.Add moduleDefinition

            let tfmAttributeClass =
                coreLibraryReference.CreateTypeReference(
                    "System.Runtime.Versioning",
                    "TargetFrameworkAttribute"
                )
                |> moduleDefinition.DefaultImporter.ImportTypeOrNull

            let tfmAttributeConstructor =
                tfmAttributeClass.CreateMemberReference(
                    ".ctor",
                    new MethodSignature(
                        CallingConventionAttributes.HasThis,
                        moduleDefinition.CorLibTypeFactory.Void,
                        [| moduleDefinition.CorLibTypeFactory.String |]
                    )
                )
                |> moduleDefinition.DefaultImporter.ImportMethod

            assembly.CustomAttributes.Add(
                CustomAttribute(
                    tfmAttributeConstructor :?> ICustomAttributeType,
                    CustomAttributeSignature(
                        Array.singleton(CustomAttributeArgument(
                            moduleDefinition.CorLibTypeFactory.String,
                            options.TargetFramework.FrameworkName
                        )),
                        Array.singleton(CustomAttributeNamedArgument(
                            CustomAttributeArgumentMemberType.Field,
                            Utf8String "FrameworkDisplayName",
                            moduleDefinition.CorLibTypeFactory.String,
                            CustomAttributeArgument(moduleDefinition.CorLibTypeFactory.String, String.empty)
                        ))
                    )
                )
            )

            ValueSome assembly
        | OutputType.Module ->
            ValueNone

    let runtimeLibraryReference =
        let runtimeLibraryName = "Wacil.Runtime"
        let assembly = AssemblyReference(runtimeLibraryName, options.RuntimeVersion)
        moduleDefinition.AssemblyReferences.Add assembly

        let runtimeMemoryClass =
            assembly.CreateTypeReference(runtimeLibraryName, "Memory") |> moduleDefinition.DefaultImporter.ImportTypeOrNull

        let runtimeMemoryClassSignature = TypeDefOrRefSignature runtimeMemoryClass

        let runtimeMemoryConstructor =
            runtimeMemoryClass.CreateMemberReference(
                ".ctor",
                new MethodSignature(
                    CallingConventionAttributes.HasThis,
                    moduleDefinition.CorLibTypeFactory.Void,
                    [|
                        moduleDefinition.CorLibTypeFactory.Int32
                        moduleDefinition.CorLibTypeFactory.Int32
                    |]
                )
            )
            |> moduleDefinition.DefaultImporter.ImportMethodOrNull

        let runtimeMemoryReadInt32 =
            runtimeMemoryClass.CreateMemberReference(
                "ReadInt32",
                new MethodSignature(
                    CallingConventionAttributes.Default,
                    moduleDefinition.CorLibTypeFactory.Int32,
                    [|
                        moduleDefinition.CorLibTypeFactory.UInt32
                        runtimeMemoryClassSignature
                        moduleDefinition.CorLibTypeFactory.UInt32
                        moduleDefinition.CorLibTypeFactory.Byte
                    |]
                )
            )
            |> moduleDefinition.DefaultImporter.ImportMethodOrNull

        let runtimeMemoryWriteInt32 =
            runtimeMemoryClass.CreateMemberReference(
                "WriteInt32",
                new MethodSignature(
                    CallingConventionAttributes.Default,
                    moduleDefinition.CorLibTypeFactory.Void,
                    [|
                        moduleDefinition.CorLibTypeFactory.UInt32
                        moduleDefinition.CorLibTypeFactory.Int32
                        runtimeMemoryClassSignature
                        moduleDefinition.CorLibTypeFactory.UInt32
                        moduleDefinition.CorLibTypeFactory.Byte
                    |]
                )
            )
            |> moduleDefinition.DefaultImporter.ImportMethodOrNull

        let runtimeMemoryGrow =
            runtimeMemoryClass.CreateMemberReference(
                "Grow",
                new MethodSignature(
                    CallingConventionAttributes.Default,
                    moduleDefinition.CorLibTypeFactory.Int32,
                    [| moduleDefinition.CorLibTypeFactory.Int32; runtimeMemoryClassSignature |]
                )
            )
            |> moduleDefinition.DefaultImporter.ImportMethodOrNull

        let runtimeTableClass =
            assembly.CreateTypeReference(runtimeLibraryName, "Table`1") |> moduleDefinition.DefaultImporter.ImportTypeOrNull

        let instantiateRuntimeTable =
            let lookup = Dictionary<RefType, _>(capacity = 2)
            fun ty ->
                match lookup.TryGetValue ty with
                | true, existing -> existing
                | false, _ ->
                    let tableElementType =
                        match ty with
                        | FuncRef -> coreSystemMulticastDelegateSignature
                        | ExternRef -> coreSystemObjectSignature

                    let specification = runtimeTableClass.MakeGenericInstanceType(tableElementType)

                    let instantiation =
                        { RuntimeLibraryTable.Specification = specification }

                    lookup[ty] <- instantiation
                    instantiation

        { Memory = runtimeMemoryClass
          MemorySignature = runtimeMemoryClassSignature
          MemoryConstructor = runtimeMemoryConstructor
          MemoryI32Load = runtimeMemoryReadInt32
          MemoryI32Store = runtimeMemoryWriteInt32
          MemoryGrow = runtimeMemoryGrow
          Table = runtimeTableClass
          InstantiatedTable = instantiateRuntimeTable }

    let getValTypeSignature (ty: ValType) =
        match ty with
        | ValType.Num I32 -> moduleDefinition.CorLibTypeFactory.Int32 :> TypeSignature
        | ValType.Num I64 -> moduleDefinition.CorLibTypeFactory.Int64
        | ValType.Num F32 -> moduleDefinition.CorLibTypeFactory.Single
        | ValType.Num F64 -> moduleDefinition.CorLibTypeFactory.Double
        | ValType.Ref ExternRef -> moduleDefinition.CorLibTypeFactory.Object
        | ValType.Ref FuncRef -> coreSystemMulticastDelegateSignature
        // System.Runtime.Intrinsics.Vector128
        | ValType.Vec _ -> raise(System.NotImplementedException "TODO: Compilation with vectors is not yet supported")

    let getFuncTypeSignature cconv (ty: FuncType) =
        // TODO: For multi-return, use out parameters
        if ty.Results.Length > 1 then failwith "TODO: Compilation of functions with multiple return values is not yet supported"

        let returnTypes =
            if ty.Results.IsEmpty
            then moduleDefinition.CorLibTypeFactory.Void :> TypeSignature
            else getValTypeSignature ty.Results[0]

        let mutable parameterTypes = ArrayBuilder<TypeSignature>.Create(ty.Parameters.Length)

        for parameter in ty.Parameters do
            parameterTypes.Add(getValTypeSignature parameter)

        MethodSignature(cconv, returnTypes, parameterTypes.ToImmutableArray())

    moduleDefinition.GetOrCreateModuleType().BaseType <- coreSystemObject

    let classDefinition =
        TypeDefinition(
            String.orEmpty options.Namespace,
            outputName,
            TypeAttributes.Sealed ||| TypeAttributes.Public ||| TypeAttributes.SequentialLayout
        )

    classDefinition.BaseType <- coreSystemObject
    moduleDefinition.TopLevelTypes.Add classDefinition

    let classDefinitionSignature = TypeDefOrRefSignature classDefinition

    let translatedModuleImports =
        let lookup = SortedList<string, ModuleImport>(input.Imports.Count, System.StringComparer.OrdinalIgnoreCase)

        let delegateConstructorSignature =
            MethodSignature(
                CallingConventionAttributes.HasThis,
                moduleDefinition.CorLibTypeFactory.Void,
                [| moduleDefinition.CorLibTypeFactory.Object; moduleDefinition.CorLibTypeFactory.IntPtr |]
            )

        for KeyValue(moduleName, imports) in input.Imports do
            let importClassDefinition =
                TypeDefinition (
                    String.empty,
                    moduleName,
                    TypeAttributes.Sealed ||| TypeAttributes.NestedPublic
                )

            importClassDefinition.BaseType <- coreSystemObject
            classDefinition.NestedTypes.Add importClassDefinition

            let importTypeSignature = TypeDefOrRefSignature importClassDefinition

            let importFieldDefinition =
                 FieldDefinition (
                     stringBuffer.Clear().Append("import_").Append(moduleName).ToString(),
                     FieldAttributes.InitOnly,
                     FieldSignature importTypeSignature
                 )

            classDefinition.Fields.Add importFieldDefinition
            let mutable importConstructorParameterTypes = ArrayBuilder<TypeSignature>.Create()
            let mutable importConstructorParameters = ArrayBuilder<_>.Create()

            let functionImportLookup = SortedList(imports.Functions.Length, System.StringComparer.Ordinal)
            for func in imports.Functions do
                let functionImportDelegate =
                    TypeDefinition(
                        String.empty,
                        func.Name,
                        TypeAttributes.NestedPublic ||| TypeAttributes.Sealed
                    )

                functionImportDelegate.BaseType <- coreSystemMulticastDelegate
                importClassDefinition.NestedTypes.Add functionImportDelegate

                let emitFunctionImportDelegateMethod name additionalFlags signature =
                    let method =
                        MethodDefinition(
                            name,
                            MethodAttributes.Public ||| MethodAttributes.HideBySig ||| additionalFlags,
                            signature
                        )

                    method.ImplAttributes <- MethodImplAttributes.Runtime
                    functionImportDelegate.Methods.Add method

                    method
                    
                emitFunctionImportDelegateMethod ".ctor" (MethodAttributes.RuntimeSpecialName ||| MethodAttributes.SpecialName) delegateConstructorSignature |> ignore

                let instanceDelegateMethodFlags = MethodAttributes.Virtual ||| MethodAttributes.NewSlot
                let functionImportSignature = getFuncTypeSignature CallingConventionAttributes.HasThis func.Type
                let functionImportInvoke = emitFunctionImportDelegateMethod "Invoke" instanceDelegateMethodFlags functionImportSignature

                // TODO: Are BeginInvoke and EndInvoke methods necessary for generated delegates?

                let functionImportField =
                    FieldDefinition(
                        func.Name,
                        FieldAttributes.InitOnly,
                        TypeDefOrRefSignature functionImportDelegate
                    )

                importClassDefinition.Fields.Add functionImportField

                importConstructorParameterTypes.Add(TypeDefOrRefSignature functionImportDelegate)
                importConstructorParameters.Add func.Name

                functionImportLookup[func.Name] <-
                    { Delegate = functionImportDelegate
                      Field = functionImportField
                      Invoke = functionImportInvoke }

            let importClassConstructor =
                MethodDefinition(
                    ".ctor",
                    MethodAttributes.Public ||| MethodAttributes.RuntimeSpecialName ||| MethodAttributes.SpecialName |||
                    MethodAttributes.HideBySig,
                    MethodSignature(
                        CallingConventionAttributes.HasThis,
                        moduleDefinition.CorLibTypeFactory.Void,
                        importConstructorParameterTypes.ToImmutableArray()
                    )
                )

            let mutable constructorParameterIndex = 1us
            for name in importConstructorParameters.ToImmutableArray() do
                importClassConstructor.ParameterDefinitions.Add(ParameterDefinition(constructorParameterIndex, name, Unchecked.defaultof<_>))
                constructorParameterIndex <- constructorParameterIndex + 1us

            importClassDefinition.Methods.Add importClassConstructor

            importClassConstructor.MethodBody <-
                let importConstructorBody = CilMethodBody importClassConstructor
                let il = importConstructorBody.Instructions
                il.Add(CilInstruction CilOpCodes.Ldarg_0)
                il.Add(CilInstruction(CilOpCodes.Call, coreSystemObjectConstructor))
                
                // TODO: Figure out what kind of duplicate imports will be allowed.
                let mutable index = 1
                for KeyValue(name, func) in functionImportLookup do
                    let storeFunctionImport = CilInstruction(CilOpCodes.Stfld, func.Field)
                    il.Add(CilInstruction CilOpCodes.Ldarg_0)
                    il.Add(CilInstruction.CreateLdarg index)
                    il.Add(CilInstruction CilOpCodes.Dup)
                    il.Add(CilInstruction(CilOpCodes.Brtrue, CilInstructionLabel storeFunctionImport))
                    il.Add(CilInstruction(CilOpCodes.Ldstr, name))
                    il.Add(CilInstruction(CilOpCodes.Newobj, coreSystemArgumentNullExceptionConstructor))
                    il.Add(CilInstruction CilOpCodes.Throw)
                    il.Add storeFunctionImport
                    index <- index + 1

                il.Add(CilInstruction CilOpCodes.Ret)
                importConstructorBody

            lookup[moduleName] <-
                { ModuleImport.Class = importClassDefinition
                  Signature = importTypeSignature
                  Field = importFieldDefinition
                  FunctionLookup = functionImportLookup }

        lookup

    let classDefinitionConstructor =
        let mutable constructorSignatureParameters = ArrayBuilder<TypeSignature>.Create translatedModuleImports.Count
        let mutable constructorParameterNames = ArrayBuilder<string>.Create translatedModuleImports.Count

        for KeyValue(name, imports) in translatedModuleImports do
            constructorSignatureParameters.Add imports.Signature
            constructorParameterNames.Add name

        let definition = MethodDefinition(
            ".ctor",
            MethodAttributes.Public ||| MethodAttributes.RuntimeSpecialName ||| MethodAttributes.SpecialName |||
            MethodAttributes.HideBySig,
            MethodSignature(
                CallingConventionAttributes.HasThis,
                moduleDefinition.CorLibTypeFactory.Void,
                constructorSignatureParameters.ToImmutableArray()
            )
        )

        let mutable sequence = 1us
        for name in constructorParameterNames.ToImmutableArray() do
            definition.ParameterDefinitions.Add(ParameterDefinition(sequence, name, Unchecked.defaultof<ParameterAttributes>))
            sequence <- Checked.(+) sequence 1us

        definition

    classDefinition.Methods.Add classDefinitionConstructor

    let classConstructorBody =
        let body = CilMethodBody classDefinitionConstructor
        let il = body.Instructions
        // Call System.Object constructor
        il.Add(CilInstruction CilOpCodes.Ldarg_0)
        il.Add(CilInstruction(CilOpCodes.Call, coreSystemObjectConstructor))

        // Set the fields corresponding to module imports
        // The order of the fields matches the order of the constructor parameters, since the lookup is sorted
        let mutable importParameterIndex = 1 // 1 is the first actual parameter of the constructor
        for KeyValue(name, import) in translatedModuleImports do
            let storeModuleImport = CilInstruction(CilOpCodes.Stfld, import.Field)
            il.Add(CilInstruction CilOpCodes.Ldarg_0)
            il.Add(CilInstruction.CreateLdarg importParameterIndex)
            il.Add(CilInstruction CilOpCodes.Dup)
            il.Add(CilInstruction(CilOpCodes.Brtrue_S, CilInstructionLabel storeModuleImport))
            il.Add(CilInstruction(CilOpCodes.Ldstr, name))
            il.Add(CilInstruction(CilOpCodes.Newobj, coreSystemArgumentNullExceptionConstructor))
            il.Add(CilInstruction CilOpCodes.Throw)
            il.Add storeModuleImport
            importParameterIndex <- importParameterIndex + 1

        body

    let classMemoryFields =
        let mutable fields = ArrayBuilder<_>.Create(input.Memories.Length)

        for i = 0 to input.Memories.Length - 1 do
            let memory = input.Memories[i]
            let memoryField =
                FieldDefinition(
                    stringBuffer.Clear().Append("memory#").Append(i).ToString(),
                    FieldAttributes.InitOnly,
                    FieldSignature runtimeLibraryReference.MemorySignature
                )

            classDefinition.Fields.Add memoryField

            fields.Add memoryField

            let instructions = classConstructorBody.Instructions
            instructions.Add(CilInstruction CilOpCodes.Ldarg_0)
            instructions.Add(CilInstruction.CreateLdcI4(Checked.int32 memory.Minimum))
            instructions.Add(CilInstruction.CreateLdcI4(ValueOption.map Checked.int32 memory.Maximum |> ValueOption.defaultValue -1))
            instructions.Add(CilInstruction(CilOpCodes.Newobj, runtimeLibraryReference.MemoryConstructor))
            instructions.Add(CilInstruction(CilOpCodes.Stfld, memoryField))

            match input.Exports.GetMemoryName(Checked.uint32 i) with
            | true, name ->
                let memoryFieldGetter =
                    MethodDefinition(
                        stringBuffer.Clear().Append("get_").Append(name).ToString(),
                        MethodAttributes.Public ||| MethodAttributes.SpecialName ||| MethodAttributes.HideBySig,
                        MethodSignature(
                            CallingConventionAttributes.HasThis,
                            runtimeLibraryReference.MemorySignature,
                            Array.empty
                        )
                    )
                
                classDefinition.Methods.Add memoryFieldGetter

                let body = CilMethodBody memoryFieldGetter
                let instructions = body.Instructions
                instructions.Add(CilInstruction CilOpCodes.Ldarg_0)
                instructions.Add(CilInstruction(CilOpCodes.Ldfld, memoryField))
                instructions.Add(CilInstruction CilOpCodes.Ret)
                memoryFieldGetter.CilMethodBody <- body

                let memoryFieldProperty =
                    PropertyDefinition(
                        name,
                        PropertyAttributes.None,
                        PropertySignature(
                            CallingConventionAttributes.HasThis,
                            runtimeLibraryReference.MemorySignature,
                            Array.empty
                        )
                    )

                memoryFieldProperty.SetSemanticMethods(memoryFieldGetter, null)
                classDefinition.Properties.Add memoryFieldProperty
            | false, _ -> ()

        fields.ToImmutableArray()

    let classTableFields =
        let fields = Array.zeroCreate input.Tables.Length
        for i = 0 to fields.Length - 1 do
            let table = input.Tables[i]
            let runtimeTableReference = runtimeLibraryReference.InstantiatedTable table.ElementType

            let generatedTableField =
                FieldDefinition(
                    stringBuffer.Clear().Append("table#").Append(i).ToString(),
                    FieldAttributes.InitOnly,
                    FieldSignature runtimeTableReference.Specification
                )

            classDefinition.Fields.Add generatedTableField

            // TODO: Generate a getter property if table is an export

            fields[i] <-
                { TranslatedTable.ElementType = table.ElementType
                  TranslatedTable.Field = generatedTableField }
        Unsafe.Array.toImmutable fields

    let generatedClassFunctions =
        let mutable functions = ArrayBuilder<_>.Create(input.Functions.Length)

        for i = 0 to input.Functions.Length - 1 do
            let func = input.Functions[i]

            let generatedFunctionName, generatedFunctionAccess =
                match input.Exports.GetFunctionName(Checked.uint32(i + input.Imports.Functions.Length)) with
                | true, existing -> existing, MethodAttributes.Public
                | false, _ -> stringBuffer.Clear().Append("function#").Append(i).ToString(), MethodAttributes.CompilerControlled

            let generatedFunctionDefinition =
                MethodDefinition(
                    generatedFunctionName,
                    generatedFunctionAccess ||| MethodAttributes.HideBySig,
                    getFuncTypeSignature CallingConventionAttributes.HasThis func.Type
                )

            classDefinition.Methods.Add generatedFunctionDefinition
            functions.Add generatedFunctionDefinition

        functions.ToImmutableArray()

    // TODO: A "setter" will need to be generated for global.set, since a ldarg_0 (the "this" pointer) cannot be inserted before value (which is already on the top of the stack)
    let translatedModuleGlobals =
        let mutable globals = Array.zeroCreate input.Globals.Length
        for i = 0 to globals.Length - 1 do
            let glbl = input.Globals[i]
            // TODO: Check export for name
            let name, access =
                stringBuffer.Clear().Append("global#").Append(i).ToString(), FieldAttributes.PrivateScope

            let mutability =
                match glbl.Type.Mutability with
                | Mutability.Const -> FieldAttributes.InitOnly
                | Mutability.Var -> Unchecked.defaultof<FieldAttributes>

            let field =
                FieldDefinition(
                    name,
                    mutability ||| access,
                    FieldSignature(getValTypeSignature glbl.Type.Type)
                )

            classDefinition.Fields.Add field

            // Note that the initializer method is not static, since CIL methods are assumed to be instance methods during translation
            let initializer =
                MethodDefinition(
                    stringBuffer.Clear().Append("init$").Append(name).ToString(),
                    MethodAttributes.CompilerControlled,
                    MethodSignature(
                        CallingConventionAttributes.HasThis,
                        field.Signature.FieldType,
                        System.Array.Empty()
                    )
                )

            classDefinition.Methods.Add initializer

            do
                let il = classConstructorBody.Instructions
                il.Add(CilInstruction CilOpCodes.Ldarg_0)
                il.Add(CilInstruction CilOpCodes.Dup)
                il.Add(CilInstruction(CilOpCodes.Call, initializer))
                il.Add(CilInstruction(CilOpCodes.Stfld, field))

            let setter =
                match glbl.Type.Mutability with
                | Mutability.Var ->
                    let definition =
                        MethodDefinition(
                            stringBuffer.Clear().Append("set$").Append(name).ToString(),
                            MethodAttributes.Static,
                            MethodSignature(
                                CallingConventionAttributes.Default,
                                moduleDefinition.CorLibTypeFactory.Void,
                                [| field.Signature.FieldType; classDefinitionSignature |]
                            )
                        )

                    classDefinition.Methods.Add definition

                    definition.ImplAttributes <- methodImplAggressiveInlining

                    definition.CilMethodBody <-
                        let body = CilMethodBody definition
                        let il = body.Instructions
                        il.Add(CilInstruction CilOpCodes.Ldarg_1)
                        il.Add(CilInstruction CilOpCodes.Ldarg_0)
                        il.Add(CilInstruction(CilOpCodes.Stfld, field))
                        il.Add(CilInstruction CilOpCodes.Ret)
                        body

                    ValueSome definition
                | Mutability.Const -> ValueNone

            globals[i] <-
                { TranslatedGlobal.Field = field
                  TranslatedGlobal.Initializer = initializer
                  TranslatedGlobal.Setter = setter }
        Unsafe.Array.toImmutable globals

    let generateClassFunctionDefinitionStatic =
        let mutable statics = Array.zeroCreate generatedClassFunctions.Length
        let parameterTypeBuilder = ImmutableArray.CreateBuilder()
        let functionImportCount = input.Imports.Functions.Length
        fun index ->
            match statics[index] with
            | ValueSome existing -> existing
            | ValueNone ->
                if index >= functionImportCount then
                    let original = generatedClassFunctions[index - functionImportCount]

                    parameterTypeBuilder.Clear()
                    parameterTypeBuilder.AddRange(original.Signature.ParameterTypes)
                    parameterTypeBuilder.Add classDefinitionSignature

                    // TODO: Maybe mark this AggressiveInlining?
                    let generatedStaticFunction =
                        MethodDefinition(
                            original.Name,
                            original.Attributes ||| MethodAttributes.Static,
                            MethodSignature(
                                CallingConventionAttributes.Default,
                                original.Signature.ReturnType,
                                parameterTypeBuilder.ToImmutable()
                            )
                        )

                    let body = CilMethodBody generatedStaticFunction
                    let il = body.Instructions
                    let parameterCount = original.Signature.ParameterTypes.Count
                    il.Add(CilInstruction.CreateLdarg parameterCount)
                    for i = 0 to parameterCount - 1 do il.Add(CilInstruction.CreateLdarg i)
                    il.Add(CilInstruction(CilOpCodes.Call, original))
                    il.Add(CilInstruction CilOpCodes.Ret)

                    generatedStaticFunction.CilMethodBody <- body

                    classDefinition.Methods.Add generatedStaticFunction
                    statics[index] <- ValueSome generatedStaticFunction
                    generatedStaticFunction
                else
                    let struct(moduleName, func) = input.Imports.Functions[index]
                    let moduleImport = translatedModuleImports[moduleName]
                    let import = moduleImport.FunctionLookup[func.Name]
                    let originalParameterTypes = import.Invoke.Signature.ParameterTypes
                    // TODO: Avoid code duplication with codegen for function definitions
                    parameterTypeBuilder.Clear()
                    parameterTypeBuilder.AddRange originalParameterTypes
                    parameterTypeBuilder.Add classDefinitionSignature

                    let helper =
                        MethodDefinition(
                            stringBuffer.Clear().Append("call_import$").Append(moduleName).Append('_').Append(func.Name).ToString(),
                            MethodAttributes.Static,
                            MethodSignature(
                                CallingConventionAttributes.Default,
                                import.Invoke.Signature.ReturnType,
                                parameterTypeBuilder.ToImmutable()
                            )
                        )

                    helper.ImplAttributes <- methodImplAggressiveInlining

                    let body = CilMethodBody helper
                    let il = body.Instructions
                    il.Add(CilInstruction.CreateLdarg originalParameterTypes.Count)
                    il.Add(CilInstruction(CilOpCodes.Ldfld, moduleImport.Field))
                    il.Add(CilInstruction(CilOpCodes.Ldfld, import.Field))
                    for i = 0 to originalParameterTypes.Count - 1 do il.Add(CilInstruction.CreateLdarg i)
                    il.Add(CilInstruction(CilOpCodes.Callvirt, import.Invoke))
                    il.Add(CilInstruction CilOpCodes.Ret)

                    helper.CilMethodBody <- body

                    classDefinition.Methods.Add helper
                    statics[index] <- ValueSome helper
                    helper

    let inline getIndexedCallee (index: Index) = generateClassFunctionDefinitionStatic(Checked.int32 index)

    //let getIndexedTable (index: Index): TranslatedTable

    let emitExpressionCode
        (instructionBlockStack: ArrayBuilder<_> ref)
        (instructionOffsetBuilder: ResizeArray<_>)
        (parameterCount: int32)
        (localVariableTypes: ImmutableArray<ValType>)
        (expression: ValidExpression)
        (body: CilMethodBody)
        =
        let (|LocalIndex|) (index: Index) =
            let i = Checked.int32 index
            if i < parameterCount then
                // Increment offsets by one, as local index 0 refers to `this`
                Arg(i + 1)
            else
                Loc(i - parameterCount)

        for ty in localVariableTypes do
            body.LocalVariables.Add(CilLocalVariable(getValTypeSignature ty))

        let mutable instructionBlockStack: byref<_> = &instructionBlockStack.contents
        let il = body.Instructions

        instructionBlockStack.Clear()
        instructionBlockStack.Add { Instructions = expression.Expression.AsMemory(); Labels = IntroducedLabelLookup.Empty }

        instructionOffsetBuilder.Clear()

        let mutable hasExplicitReturn = false

        let inline pushMemoryField i =
            il.Add(CilInstruction CilOpCodes.Ldarg_0)
            il.Add(CilInstruction(CilOpCodes.Ldfld, classMemoryFields[i]))

        let inline pushMemArg (arg: MemArg) =
            il.Add(CilInstruction.CreateLdcI4(int32 arg.Offset))
            il.Add(CilInstruction.CreateLdcI4(int32 arg.Alignment.Power))

        /// Maps indices that refer to WASM instructions to CIL labels.
        let labels = Dictionary<int, CilInstructionLabel>(expression.LabelIndices.Length)
        for index in expression.LabelIndices do labels.Add(index, CilInstructionLabel())

        while not instructionBlockStack.IsEmpty do
            let block = instructionBlockStack.LastRef()
            if block.Instructions.IsEmpty then
                let _ = instructionBlockStack.Pop()
                ()
            else
                let instruction = block.Instructions.Span[0]

                match instruction.Instruction with
                | Instruction.Normal normal ->
                    match normal with
                    | Br label ->
                        il.Add(CilInstruction(CilOpCodes.Br, labels[block.Labels.GetLabel(label).Index]))
                    | Call callee ->
                        // Parameters are already on the stack in the correct order, so "this" pointer needs to be inserted last
                        il.Add(CilInstruction CilOpCodes.Ldarg_0)
                        il.Add(CilInstruction(CilOpCodes.Call, getIndexedCallee callee))
                    | Drop -> il.Add(CilInstruction CilOpCodes.Pop)
                    | LocalGet(LocalIndex index) ->
                        match index with
                        | Arg i -> il.Add(CilInstruction.CreateLdarg i)
                        | Loc i -> il.Add(CilInstruction.CreateLdloc i)
                    | LocalSet(LocalIndex index) ->
                        match index with
                        | Arg i -> il.Add(CilInstruction.CreateStarg i)
                        | Loc i -> il.Add(CilInstruction.CreateStloc i)
                    | GlobalGet index ->
                        // TODO: Check for imported globals
                        il.Add(CilInstruction CilOpCodes.Ldarg_0)
                        il.Add(CilInstruction(CilOpCodes.Ldfld, translatedModuleGlobals[Checked.int32 index].Field))
                    | GlobalSet index ->
                        // TODO: Check for imported globals
                        il.Add(CilInstruction CilOpCodes.Ldarg_0)
                        il.Add(CilInstruction(CilOpCodes.Call, translatedModuleGlobals[Checked.int32 index].Setter.Value))
                    | I32Load arg ->
                        // Top of stack is address to load, which is first parameter
                        pushMemoryField 0
                        pushMemArg arg
                        il.Add(CilInstruction(CilOpCodes.Call, runtimeLibraryReference.MemoryI32Load))
                    | I32Store arg ->
                        // Stack contains the value to store on top of the address
                        pushMemoryField 0
                        pushMemArg arg
                        il.Add(CilInstruction(CilOpCodes.Call, runtimeLibraryReference.MemoryI32Store))
                    | MemoryGrow ->
                        // Top of the stack is the size delta
                        pushMemoryField 0
                        il.Add(CilInstruction(CilOpCodes.Call, runtimeLibraryReference.MemoryGrow))
                    | I32Const value -> il.Add(CilInstruction.CreateLdcI4 value)
                    | I64Const value ->
                        if (value >>> 32) &&& 0xFFFF_FFFFL = 0L then
                            il.Add(CilInstruction.CreateLdcI4(int32 value))
                            il.Add(CilInstruction CilOpCodes.Conv_I8)
                        else
                            il.Add(CilInstruction(CilOpCodes.Ldc_I8, value))
                    | F32Const value -> il.Add(CilInstruction(CilOpCodes.Ldc_R4, value))
                    | F64Const value -> il.Add(CilInstruction(CilOpCodes.Ldc_R8, value))
                    | I32Add -> il.Add(CilInstruction CilOpCodes.Add)
                    | I32Sub -> il.Add(CilInstruction CilOpCodes.Sub)
                    | bad -> failwithf "Compilation of %A not yet supported" bad
                | Instruction.Structured structured ->
                    match instruction.Kind with
                    | Structured(labels, body) ->
                        match structured.Kind with
                        | Block ->
                            assert(body.Length = 1)
                            instructionBlockStack.Add { Instructions = body[0].AsMemory(); Labels = labels }
                        | bad -> failwithf "Unsupported structured kind compile fix later %A" bad
                    | Normal -> failwithf "%A should not be marked as a normal isntruction" instruction

                match labels.TryGetValue instruction.Index with
                | false, _ -> ()
                | true, label ->
                    let nop = CilInstruction CilOpCodes.Nop
                    il.Add nop
                    label.Instruction <- nop

                block.Instructions <- block.Instructions.Slice(1)

        if not hasExplicitReturn then
            // TODO: Implicit return could be a branch target, but does the above code already cover that case?
            il.Add(CilInstruction CilOpCodes.Ret)

    do
        let instructionBlockStack = ArrayBuilder<_>.Create() |> ref
        let instructionOffsetBuilder = ResizeArray()
        for i = 0 to input.Functions.Length - 1 do
            let func = input.Functions[i]
            let definition = generatedClassFunctions[i]
            let body = CilMethodBody definition
            emitExpressionCode instructionBlockStack instructionOffsetBuilder func.Type.Parameters.Length func.LocalTypes func.Body body
            definition.CilMethodBody <- body

        for i = 0 to translatedModuleGlobals.Length - 1 do
            let translated = translatedModuleGlobals[i]
            let original = input.Globals[i]
            let body = CilMethodBody translated.Initializer
            emitExpressionCode instructionBlockStack instructionOffsetBuilder 0 ImmutableArray.Empty original.Value body
            translated.Initializer.CilMethodBody <- body

    match input.Start with
    | ValueSome index ->
        let il = classConstructorBody.Instructions
        // Can call non-static version of start function here, since WebAssembly does not allow start functions to accept parameters.
        il.Add(CilInstruction CilOpCodes.Ldarg_0)
        il.Add(CilInstruction(CilOpCodes.Call, generatedClassFunctions[index]))
    | ValueNone -> ()

    // Done generating code for constructor
    classConstructorBody.Instructions.Add(CilInstruction CilOpCodes.Ret)
    classDefinitionConstructor.CilMethodBody <- classConstructorBody

    moduleDefinition

let compileToStream options input (stream: System.IO.Stream) =
    if isNull stream then nullArg (nameof stream)
    try
        if not stream.CanWrite then invalidArg (nameof stream) "Destination stream must support writing"
        (compileToModuleDefinition options input).Write stream
    finally
        stream.Close()
