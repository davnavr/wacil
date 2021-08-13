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

    let translateMethodBody locinit funcParamCount localTypesBuilder { Code.Locals = locals; Body = body } =
        // TODO: Figure out how to handle branching.
        let blocks = List()
        let instrs' = ImmutableArray.CreateBuilder<Cil.Instruction>()
        // TODO: Make custom label list class.
        let labels = List<Cil.Label ref>() // NOTE: This assumes that, within a block, labels are numbered sequentially
        let lindices = Stack<int32>()

        let commitInstructionList() =
            blocks.Add(InstructionBlock.ofBlock(instrs'.ToImmutable()))
            instrs'.Clear()

        let mutable commit = commitInstructionList

        let inline translateVariableInstr argi loci (Index i) =
            let i' = Checked.uint16 i
            if i < funcParamCount
            then argi(Checked.uint16 i) |> instrs'.Add
            else loci(LocalVarIndex.locali i') |> instrs'.Add

        let inline branchToLabel opcode str (Index index: Index<IndexKinds.Label>) =
            instrs'.Add(Instruction.branchingRef opcode str BranchKind.Long labels.[labels.Count - 1 - Checked.int32 index])

        let createBranchBlock() =
            let i, prev = labels.Count, commit
            lindices.Push i
            labels.Add(ref Unchecked.defaultof<_>)

            commit <- fun() ->
                let struct(l, b) = InstructionBlock.label(InstructionBlock.ofBlock(instrs'.ToImmutable()))
                instrs'.Clear()
                labels.[i].Value <- l
                labels.RemoveRange(lindices.Pop(), labels.Count - i)
                blocks.Add b
                commit <- prev

        for instr in body do
            match instr with
            | { Opcode = 0x01uy; Arguments = InstructionArguments.Nothing } -> instrs'.Add Cil.Instructions.nop // nop
            | { Opcode = 0x03uy; Arguments = InstructionArguments.BlockType _ } -> // loop
                createBranchBlock() // TODO: Fix, make sure label is before loop body
            | { Opcode = 0x04uy; Arguments = InstructionArguments.BlockType _ } -> // if
                let prev = commit
                let l = ref Unchecked.defaultof<_>
                instrs'.Add(Instruction.branchingRef Opcode.Brfalse (StackBehavior.PopOrPush -1y) BranchKind.Long l)

                commit <- fun() ->
                    let struct(l', b) = InstructionBlock.label InstructionBlock.empty
                    blocks.Add b
                    l.Value <- l'
                    commit <- prev

                createBranchBlock()
            | { Opcode = 5uy; Arguments = InstructionArguments.Nothing } -> () // else
            | { Opcode = 0x0Cuy; Arguments = InstructionArguments.LabelIndex i } ->
                branchToLabel Opcode.Br (StackBehavior.PopOrPush 0y) i
            | { Opcode = 0x0Buy; Arguments = InstructionArguments.Nothing } ->
                commit() // end
            | { Opcode = 0x20uy; Arguments = InstructionArguments.LocalIndex i } ->
                translateVariableInstr Shortened.ldarg Shortened.ldloc i // local.get
            | { Opcode = 0x21uy; Arguments = InstructionArguments.LocalIndex i } ->
                translateVariableInstr Shortened.starg Shortened.stloc i // local.set
            | { Opcode = 0x41uy; Arguments = InstructionArguments.I32 n } -> instrs'.Add(Shortened.ldc_i4 n) // i32.const
            | { Opcode = 0x42uy; Arguments = InstructionArguments.I64 n } -> instrs'.Add(ldc_i8 n) // i64.const
            | { Opcode = 0x52uy; Arguments = InstructionArguments.Nothing } ->
                instrs'.Add ceq
                instrs'.Add ldc_i4_0
                instrs'.Add ceq
            | { Opcode = 0x6Auy | 0x7Cuy; Arguments = InstructionArguments.Nothing } -> instrs'.Add add // i32.add, i64.add
            | { Opcode = 0x6Buy | 0x7Duy; Arguments = InstructionArguments.Nothing } -> instrs'.Add sub // i32.sub, i64.sub
            | { Opcode = 0x6Cuy | 0x7Euy; Arguments = InstructionArguments.Nothing } -> instrs'.Add mul // i32.mul, i64.mul
            | _ -> failwithf "TODO: Error for cannot translate unknown opcode 0x%02X" instr.Opcode

        if instrs'.Count > 0 then commit()

        blocks.Add(InstructionBlock.singleton ret)

        MethodBody.create locinit ValueNone (getMethodLocals localTypesBuilder locals) blocks |> ValueSome

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
                    // TODO: Check options to see if skip init locals should be used.
                    let body = translateMethodBody InitLocals (Checked.uint32 funct.Parameters.Length) locals code.[i]
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
