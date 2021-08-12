module wacil.Generator

open System.Collections.Generic
open System.Collections.Immutable
open System.Runtime.CompilerServices

open FSharpIL.Metadata
open FSharpIL.Metadata.Cil
open FSharpIL.Metadata.Tables

open FSharpIL.Cli
open FSharpIL.PortableExecutable

open FSharpIL.Writing
open FSharpIL.Writing.Cil

open Wasm.Format
open Wasm.Format.InstructionSet
open Wasm.Format.Types

type FileType = | Assembly | Netmodule

[<RequireQualifiedAccess>]
module FileType =
    let extension fileType =
        match fileType with
        | Assembly _ -> "dll"
        | Netmodule -> "netmodule"

type Options =
    { ModuleFileName: string
      FileType: FileType
      HighEntropyVA: bool
      TargetFramework: string
      MainClassName: string }

[<RequireQualifiedAccess>]
module Generate =
    let addCoreAssembly (metadata: CliModuleBuilder) =
        let mscorlib =
            { ReferencedAssembly.Version = AssemblyVersion(5us, 0us, 0us, 0us)
              PublicKeyOrToken = PublicKeyToken(0xb0uy, 0x3fuy, 0x5fuy, 0x7fuy, 0x11uy, 0xd5uy, 0x0auy, 0x3auy)
              Name = FileName.ofStr "System.Runtime"
              Culture = ValueNone
              HashValue = ImmutableArray.Empty }
        do metadata.ReferenceAssembly mscorlib

        let system = ValueSome(Identifier.ofStr "System")

        let object =
            { TypeReference.TypeName = Identifier.ofStr "Object"
              TypeNamespace = system
              Flags = ValueNone
              ResolutionScope = TypeReferenceParent.Assembly mscorlib }
        do metadata.ReferenceType(ReferencedType.Reference object) |> ValidationResult.get |> ignore

        let tfmattr =
            { TypeReference.TypeName = Identifier.ofStr "TargetFrameworkAttribute"
              TypeNamespace = ValueSome(Identifier.ofStr "System.Runtime.Versioning")
              Flags = ValueNone
              ResolutionScope = TypeReferenceParent.Assembly mscorlib }
            |> ReferencedType.Reference
            |> metadata.ReferenceType
            |> ValidationResult.get

        {| Assembly = mscorlib
           Object = object
           TargetFrameworkAttribute =
             {| Constructor =
                  ReferencedMethod.Constructor (
                      visibility = ExternalVisibility.Public,
                      parameterTypes = ImmutableArray.Create(ParameterType.T PrimitiveType.String)
                  )
                  |> tfmattr.ReferenceMethod
                  |> ValidationResult.get |} |}

    let addAssemblyDefinition options (metadata: CliModuleBuilder) =
        match options.FileType with
        | Assembly ->
            let assembly =
                { DefinedAssembly.Name = FileName.ofStr options.ModuleFileName
                  Version = AssemblyVersion(1us, 0us, 0us, 0us) // TODO: Have option to set version of assembly.
                  PublicKey = ImmutableArray.Empty
                  Culture = ValueNone }
            do metadata.DefineAssembly assembly |> ValidationResult.get |> ignore
            ValueSome assembly
        | Netmodule -> ValueNone

    let setTargetFramework options tfmattr (metadata: CliModuleBuilder) =
        match metadata.Assembly with
        | Some _ ->
            validated {
                do! metadata.SetTargetFramework(options.TargetFramework, CustomAttributeCtor.Referenced tfmattr)
            }
            |> ValidationResult.get
        | None -> ()

    let getValueType (vtype: ValType) =
        match vtype with
        | ValType.NumType nt ->
            match nt with
            | NumType.I32 -> PrimitiveType.I4
            | NumType.I64 -> PrimitiveType.I8
            | NumType.F32 -> PrimitiveType.R4
            | NumType.F64 -> PrimitiveType.R8
        | ValType.RefType rt ->
            match rt with
            | RefType.ExternRef -> PrimitiveType.Object
            | RefType.FuncRef -> failwith "TODO: Consider using System.Delegate or function pointers for FuncRef"

    let getReturnType (rtype: ResultType) =
        match rtype.Length with
        | 0 -> ReturnType.Void'
        | 1 -> ReturnType.T(getValueType rtype.[0])
        | _ -> failwithf "TODO: Add support for multiple return types (use tuple types?)"

    let getParameterTypes (parameters: ResultType) =
        let mutable parameters' = Array.zeroCreate parameters.Length
        for i = 0 to parameters'.Length - 1 do
            parameters'.[i] <- ParameterType.T(getValueType parameters.[i])
        Unsafe.As<_, ImmutableArray<ParameterType>> &parameters'

    let getMethodLocals (localTypesBuilder: ImmutableArray<_>.Builder) (locals: ImmutableArray<Locals>) =
        localTypesBuilder.Clear()

        for l in locals do
            let t' = getValueType l.Type
            for _ = 0 to Checked.int32 l.Count do localTypesBuilder.Add(CliType.toLocalType t')

        localTypesBuilder.ToImmutable() |> LocalVariables.Locals

    let translateMethodBody funcParamCount localTypesBuilder { Code.Locals = locals; Body = body } =
        // TODO: Check options to see if skip init locals should be used.
        MethodBody.create InitLocals ValueNone (getMethodLocals localTypesBuilder locals) [
            seq {
                for instr in body do
                    match instr with
                    | { Opcode = 0x0Buy; Arguments = InstructionArguments.Nothing } -> () // end
                    | { Opcode = 0x20uy; Arguments = InstructionArguments.LocalIndex(Index i) } when i < funcParamCount ->
                        // local.get
                        let i' = Checked.uint16 i
                        if i < funcParamCount
                        then Shortened.ldarg(Checked.uint16 i)
                        else Shortened.ldloc(LocalVarIndex.locali i')
                    | { Opcode = 0x41uy; Arguments = InstructionArguments.I32 n } -> Shortened.ldc_i4 n // i32.const
                    | { Opcode = 0x6Auy; Arguments = InstructionArguments.Nothing } -> add // i32.add
                    | _ -> failwithf "TODO: Error for unknown opcode %A" instr.Opcode

                ret
            }
            |> InstructionBlock.ofSeq
        ]
        |> ValueSome

    let addTranslatedFunctions (members: DefinedTypeMembers) sections =
        match sections with
        | { TypeSection = ValueSome types
            FunctionSection = ValueSome funcs
            CodeSection = ValueSome code }
            ->
            let exports = ValueOption.map getModuleExports sections.ExportSection
            let methods = Dictionary<Index<IndexKinds.Func>, MethodTok> funcs.Length
            let locals = ImmutableArray.CreateBuilder<LocalType>() // Shared to avoid extra allocations.

            let getFunctionName =
                match exports with
                | ValueSome exports' ->
                    ModuleExports.tryGetFunction exports' >> ValueOption.map (fun export -> MethodName.ofStr export.Name)
                | ValueNone -> fun _ -> ValueNone

            for i = 0 to funcs.Length - 1 do
                let i' = funcs.First + uint32 i
                let funct = &types.[funcs.[i'].Type]
                let func' =
                    let visibility, name =
                        match getFunctionName i' with
                        | ValueSome name' -> MethodDefFlags.Public, name'
                        | ValueNone -> MethodDefFlags.Private, MethodName.ofStr ("func#" + string i)
                    DefinedMethod (
                        MethodImplFlags.IL,
                        MethodDefFlags.Static ||| visibility,
                        FSharpIL.Metadata.Signatures.MethodThis.NoThis,
                        getReturnType funct.Results,
                        name,
                        getParameterTypes funct.Parameters,
                        Parameter.emptyList
                    )

                // TODO: Use method dictionary when generating call instructions.
                methods.[i'] <-
                    let body = translateMethodBody (Checked.uint32 funct.Parameters.Length) locals code.[i]
                    let token = members.DefineMethod(func', body, ValueNone) |> ValidationResult.get
                    token.Token

            methods
        | _ -> failwith "TODO: How to deal with missing sections?"

    let toPE (ValidatedModule file) options = // TODO: Set file to Exe if start function is defined.
        let sections = getKnownSections file
        let extension = FileType.extension options.FileType

        // TODO: Figure out how to deterministically generate MVID
        let metadata = CliModuleBuilder(Identifier.ofStr(options.ModuleFileName + "." + extension))
        // TODO: Figure out how to allow usage of other core assemblies?
        let mscorlib = addCoreAssembly metadata
        do addAssemblyDefinition options metadata |> ignore

        let members =
            let module' =
                { TypeDefinition.TypeName = Identifier.ofStr options.ModuleFileName
                  TypeNamespace = ValueNone // TODO: If module name is separated by periods, make the first few things the namespace
                  Flags = TypeDefFlags.Public ||| TypeDefFlags.Sealed ||| TypeDefFlags.Abstract
                  Extends =
                    ReferencedType.Reference mscorlib.Object
                    |> NamedType.ReferencedType 
                    |> ClassExtends.Named
                  EnclosingClass = ValueNone }
                |> DefinedType.Definition

            metadata.DefineType(module', ValueNone) |> ValidationResult.get

        let _ = addTranslatedFunctions members sections

        // TODO: Add FSharpIL function to allow translation of CliModuleBuilder to section contents
        if not options.HighEntropyVA then failwith "// TODO: Allow setting of ASLR flag in optional header."
        BuildPE.ofModuleBuilder FileCharacteristics.IsDll metadata
