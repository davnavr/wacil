module wacil.Generator

open System
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
    type CoreRuntimeAssembly =
        { Assembly : ReferencedAssembly
          Marshal:
            {| AllocHGlobal: MethodTok<ReferencedType, ReferencedMethod>
               ReAllocHGlobal: MethodTok<ReferencedType, ReferencedMethod> |}
          Object: {| Type: TypeReference; Constructor: MethodTok<ReferencedType, ReferencedMethod> |}
          ValueType: TypeReference
          TargetFrameworkAttribute:
            {| Constructor: MethodTok<TypeReference<TypeKinds.SealedClass>, MethodReference<MethodKinds.ObjectConstructor>> |} }

    let addCoreAssembly (metadata: CliModuleBuilder) =
        let mscorlib =
            { ReferencedAssembly.Version = AssemblyVersion(5us, 0us, 0us, 0us)
              PublicKeyOrToken = PublicKeyToken(0xCCuy, 0x7Buy, 0x13uy, 0xFFuy, 0xCDuy, 0x2Duy, 0xDDuy, 0x51uy)
              Name = FileName.ofStr "netstandard"
              Culture = ValueNone
              HashValue = ImmutableArray.Empty }
        do metadata.ReferenceAssembly mscorlib

        let system = ValueSome(Identifier.ofStr "System")
        let inline referenceSystemType name =
            { TypeReference.TypeName = Identifier.ofStr name
              TypeNamespace = system
              Flags = ValueNone
              ResolutionScope = TypeReferenceParent.Assembly mscorlib }

        let object = referenceSystemType "Object"
        let object' = metadata.ReferenceType(ReferencedType.Reference object) |> ValidationResult.get

        let vtype = referenceSystemType "ValueType"
        do metadata.ReferenceType(ReferencedType.Reference vtype) |> ValidationResult.get |> ignore

        let tfmattr =
            TypeReference.SealedClass (
                TypeReferenceParent.Assembly mscorlib,
                ValueSome(Identifier.ofStr "System.Runtime.Versioning"),
                Identifier.ofStr "TargetFrameworkAttribute"
            )
            |> metadata.ReferenceType
            |> ValidationResult.get

        let marshal =
            { TypeReference.TypeName = Identifier.ofStr "Marshal"
              TypeNamespace = ValueSome(Identifier.ofStr "System.Runtime.InteropServices")
              Flags = ValueNone
              ResolutionScope = TypeReferenceParent.Assembly mscorlib }
            |> ReferencedType.Reference
            |> metadata.ReferenceType
            |> ValidationResult.get

        { Assembly = mscorlib
          Marshal =
            {| AllocHGlobal =
                ReferencedMethod.Static (
                    ExternalVisibility.Public,
                    ReturnType.T PrimitiveType.I,
                    MethodName.ofStr "AllocHGlobal",
                    ImmutableArray.Create(ParameterType.T PrimitiveType.I4)
                )
                |> marshal.ReferenceMethod
                |> ValidationResult.get
               ReAllocHGlobal =
                 ReferencedMethod.Static (
                     ExternalVisibility.Public,
                     ReturnType.T PrimitiveType.I,
                     MethodName.ofStr "ReAllocHGlobal",
                     ImmutableArray.Create(ParameterType.T PrimitiveType.I, ParameterType.T PrimitiveType.I)
                 )
                 |> marshal.ReferenceMethod
                 |> ValidationResult.get |}
          Object =
            {| Type = object
               Constructor =
                 ReferencedMethod.Constructor(ExternalVisibility.Public, ImmutableArray.Empty)
                 |> object'.ReferenceMethod
                 |> ValidationResult.get |}
          ValueType = vtype
          TargetFrameworkAttribute =
            {| Constructor =
                 ReferencedMethod.Constructor (
                     visibility = ExternalVisibility.Public,
                     parameterTypes = ImmutableArray.Create(ParameterType.T PrimitiveType.String)
                 )
                 |> tfmattr.ReferenceMethod
                 |> ValidationResult.get |} }

    let addAssemblyDefinition options (metadata: CliModuleBuilder) =
        match options.FileType with
        | Assembly ->
            let assembly =
                { DefinedAssembly.Name = FileName.ofStr options.ModuleFileName
                  Version = AssemblyVersion.Zero // TODO: Have option to set version of assembly.
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

    type MemoryType =
        { Definition: DefinedType
          Type: CliType
          Token: TypeTok
          Constructor: MethodTok<DefinedType, DefinedMethod>
          Grow: MethodTok
          LoadI32: MethodTok<DefinedType, DefinedMethod>
          StoreI32: MethodTok<DefinedType, DefinedMethod> }

    let addMemoryType
        mscorlib
        module'
        exports
        (metadata: CliModuleBuilder)
        =
        // NOTE: When multi-memory proposal of WebAssembly is implemented, consider making Mem a ref struct
        let mem =
            { TypeDefinition.TypeName = Identifier.ofStr "Mem"
              TypeNamespace = ValueNone
              Flags =
                match exports with
                | ValueSome _ -> TypeDefFlags.NestedPublic
                | ValueNone -> TypeDefFlags.NestedPrivate
                ||| TypeDefFlags.Sealed
              Extends =
                ReferencedType.Reference mscorlib.Object.Type // .ValueType
                |> NamedType.ReferencedType
                |> ClassExtends.Named
              EnclosingClass = ValueSome module' }
            |> DefinedType.Definition

        let members = metadata.DefineType(mem, ValueNone) |> ValidationResult.get

        let addInternalField attrs name ftype =
            members.DefineField (
                DefinedField.Instance (
                    MemberVisibility.Private,
                    attrs,
                    Identifier.ofStr name,
                    ftype
                ),
                ValueNone
            )
            |> ValidationResult.get

        // Only one region of memory is used at a time, as it would be a pain to write an int64 between two regions of memory.
        let memory = addInternalField FieldAttributes.None "memory" PrimitiveType.I
        let length = addInternalField FieldAttributes.None "length" PrimitiveType.I4
        let capacity = addInternalField FieldAttributes.None "capacity" PrimitiveType.I4
        let min = addInternalField FieldAttributes.InitOnly "minimum" PrimitiveType.U4
        let max = addInternalField FieldAttributes.InitOnly "maximum" PrimitiveType.U4
        //let lock = addInternalField FieldAttributes.InitOnly "lock" PrimitiveType.Object members

        let inline defineMemoryHelper rtype name ptypes parameters initl locals body =
            let body' = MethodBody.create initl ValueNone locals body

            members.DefineMethod (
                DefinedMethod.Instance (
                    MemberVisibility.Assembly,
                    MethodAttributes.HideBySig,
                    rtype,
                    MethodName.ofStr name,
                    ptypes,
                    parameters
                ),
                ValueSome body',
                ValueNone
            )
            |> ValidationResult.get

        /// Fills allocated memory with zeroed bytes, since AllocHGlobal does not.
        let zero =
            InstructionBlock.ofSeq [|
                ldarg_0
                ldfld memory.Token
                ldc_i4_0
                ldarg_0
                ldfld capacity.Token
                conv_ovf_u4
                Prefixes.unaligned_ 1uy
                initblk
                ret
            |]
            |> Seq.singleton
            |> defineMemoryHelper
                ReturnType.Void'
                "ZeroOut"
                ImmutableArray.Empty
                Parameter.emptyList
                InitLocals
                LocalVariables.Null

        let ctor =
            let body = MethodBody.create InitLocals ValueNone LocalVariables.Null [|
                InstructionBlock.ofList [
                    ldarg_0
                    Cil.Instructions.call mscorlib.Object.Constructor.Token

                    let inline setSizeField argi field = [
                        ldarg_0
                        Shortened.ldarg argi
                        ldc_i4(int32 Wasm.Format.PageSize)
                        mul
                        stfld field
                    ]

                    yield! setSizeField 1us min.Token
                    yield! setSizeField 2us max.Token

                    // TODO: Exception if max > min

                    ldarg_0
                    dup
                    ldfld min.Token
                    conv_ovf_i4_un
                    stfld capacity.Token

                    ldarg_0
                    dup
                    ldfld capacity.Token
                    stfld length.Token

                    ldarg_0
                    dup
                    ldfld capacity.Token
                    Cil.Instructions.call mscorlib.Marshal.AllocHGlobal.Token
                    stfld memory.Token

                    ldarg_0
                    Cil.Instructions.call zero.Token

                    ret
                ]
            |]

            let u2' = ParameterType.T PrimitiveType.U2

            members.DefineMethod (
                DefinedMethod.Constructor (
                    MemberVisibility.Assembly,
                    MethodAttributes.HideBySig,
                    ImmutableArray.Create(u2', u2'),
                    fun i _ ->
                        match i with
                        | 0 -> "minPageCount"
                        | _ -> "maxPageCount"
                        |> Identifier.ofStr
                        |> Parameter.named
                ),
                ValueSome body,
                ValueNone
            )
            |> ValidationResult.get

        let pgcount =
            let getter =
                DefinedMethod.Instance (
                    MemberVisibility.Public,
                    MethodAttributes.HideBySig ||| MethodAttributes.SpecialName,
                    ReturnType.T PrimitiveType.I4,
                    MethodName.ofStr "get_PageCount",
                    ImmutableArray.Empty,
                    Parameter.emptyList
                )

            let getter' = MethodBody.ofSeq [|
                ldarg_0
                ldfld length.Token
                ldc_i4(int32 Wasm.Format.PageSize)
                div
                ret
            |]

            members.DefineProperty (
                Identifier.ofStr "PageCount",
                ValueSome(getter :> DefinedMethod, ValueSome getter', ValueNone),
                ValueNone,
                List.empty,
                ValueNone
            )
            |> ValidationResult.get

        let grow =
            defineMemoryHelper
                (ReturnType.T PrimitiveType.I4)
                "Grow"
                (ImmutableArray.Create(ParameterType.T PrimitiveType.I4))
                (fun _ _ -> Parameter.named(Identifier.ofStr "by"))
                InitLocals
                LocalVariables.Null
                [|
                    let struct(negative, negative') =
                        InstructionBlock.ofSeq [|
                            // when value is negative
                            // TODO: Throw exception instead?
                            ldc_i4_m1
                            ret
                        |]
                        |> InstructionBlock.label

                    // TODO: Add a try block to return -1 if memory cannot be allocated.
                    let struct(returnl, returnl') =
                        InstructionBlock.ofSeq [|
                            ldarg_0
                            Cil.Instructions.call pgcount.Getter.Value.Token
                            ret
                        |]
                        |> InstructionBlock.label

                    InstructionBlock.ofSeq [|
                        ldarg_1
                        ldc_i4_0
                        blt_s negative
                        // when value is positive or zero

                        ldarg_1
                        ldc_i4_0
                        beq returnl

                        // when value is positive
                        ldarg_0
                        dup
                        ldfld length.Token
                        ldarg_1
                        ldc_i4(int32 Wasm.Format.PageSize)
                        mul_ovf
                        add_ovf
                        stfld length.Token

                        ldarg_0
                        ldfld length.Token
                        ldarg_0
                        ldfld capacity.Token
                        ble_s returnl

                        // when value requires a rellocation of memory
                        // TODO: If possible, multiply capacity by 2 instead.
                        ldarg_0
                        dup
                        ldfld length.Token
                        stfld capacity.Token

                        ldarg_0
                        dup
                        ldfld memory.Token
                        ldarg_0
                        ldfld capacity.Token
                        conv_i
                        Cil.Instructions.call mscorlib.Marshal.ReAllocHGlobal.Token
                        stfld memory.Token

                        ldarg_0
                        Cil.Instructions.call zero.Token
                    |]

                    returnl'
                    negative'
                |]

        let checkMemoryOffset =
            let locals =
                CliType.toLocalType PrimitiveType.I
                |> ImmutableArray.Create
                |> LocalVariables.Locals

            let body = MethodBody.create InitLocals ValueNone locals [|
                let struct(ok, ok') = InstructionBlock.label (InstructionBlock.singleton ret)

                let struct(error, error') =
                    InstructionBlock.ofSeq [|
                        // TODO: Fix FSharpIL bug where string literal that is too long results in generation of invalid index.
                        ldstr "Indexed memory out of bounds"
                        throw
                    |]
                    |> InstructionBlock.label

                InstructionBlock.ofSeq [|
                    ldarg_1
                    ldarg_2
                    add_ovf
                    stloc_0

                    ldloc_0
                    ldc_i4_0
                    blt_s error

                    ldloc_0
                    ldarg_0
                    ldfld length.Token
                    conv_ovf_i
                    bge_s error

                    br_s ok
                |]

                error'
                ok'
            |]

            members.DefineMethod (
                DefinedMethod.Instance (
                    MemberVisibility.Private,
                    MethodAttributes.HideBySig,
                    ReturnType.Void',
                    MethodName.ofStr "CheckOffset",
                    ImmutableArray.Create(ParameterType.T PrimitiveType.I, ParameterType.T PrimitiveType.I4),
                    fun i _ ->
                        match i with
                        | 0 -> "offset"
                        | _ -> "size"
                        |> Identifier.ofStr
                        |> Parameter.named
                ),
                ValueSome body,
                ValueNone
            )
            |> ValidationResult.get

        let valueOffsetLocals =
            ImmutableArray.Create (
                CliType.toLocalType PrimitiveType.I,
                CliType.toLocalType PrimitiveType.I // TODO: Should be byte pointer
            )
            |> LocalVariables.Locals

        let setValueOffset = InstructionBlock.ofSeq [|
            ldarg_2
            ldarg_1
            conv_ovf_i4_un
            mul_ovf
            conv_ovf_i
            stloc_0

            ldarg_0
            ldfld memory.Token
            ldloc_0
            add_ovf
            stloc_1
        |]

        let checkValueOffset size = InstructionBlock.ofSeq [|
            ldarg_0
            ldloc_0
            Shortened.ldc_i4 size
            Cil.Instructions.call checkMemoryOffset.Token
        |]

        let memoryLoadParameters = ImmutableArray.Create(ParameterType.T PrimitiveType.U4, ParameterType.T PrimitiveType.I4)

        let defineMemoryLoad =
            let parameters i _ = Identifier.ofStr(if i = 0 then "alignment" else "offset") |> Parameter.named

            fun vtype vsize name initl body ->
                defineMemoryHelper
                    (ReturnType.T vtype)
                    name
                    memoryLoadParameters
                    parameters
                    initl
                    valueOffsetLocals
                    (seq { setValueOffset; checkValueOffset vsize; yield! body })

        let inline loadIntegerValue size = InstructionBlock.ofSeq (seq {
            for i = 1 to size do
                ldloc_1
                if i > 1 then
                    Shortened.ldc_i4(i - 1)
                    add_ovf
                ldind_u1
                conv_u4
                if i > 1 then
                    Shortened.ldc_i4((i - 1) * 8)
                    shl
                if i > 1 then add_ovf_un
            ret
        })

        let defineMemoryStore =
            let parameters i _ =
                match i with
                | 0 -> "alignment"
                | 1 -> "offset"
                | _ -> "value"
                |> Identifier.ofStr
                |> Parameter.named

            fun vtype vsize name initl body ->
                defineMemoryHelper
                    ReturnType.Void'
                    name
                    (ImmutableArray.Create (
                        ParameterType.T PrimitiveType.U4,
                        ParameterType.T PrimitiveType.I4,
                        ParameterType.T vtype
                    ))
                    parameters
                    initl
                    valueOffsetLocals
                    (seq { setValueOffset; checkValueOffset vsize; yield! body })

        let inline storeIntegerValue size = InstructionBlock.ofSeq (seq {
            for i = 1 to size do
                ldloc_1
                if i > 1 then
                    Shortened.ldc_i4(i - 1)
                    add_ovf

                ldarg_3
                if i > 1 then
                    Shortened.ldc_i4((i - 1) * 8)
                    shl
                ldc_i4 0xFF
                Cil.Instructions.``and``
                conv_u1

                stind_i1
            ret
        })

        let mem' = NamedType.DefinedType mem

        { Definition = mem
          Type = CliType.Class mem'
          Token = TypeTok.Named mem'
          Constructor = ctor
          Grow = grow.Token
          LoadI32 = defineMemoryLoad PrimitiveType.I4 4 "LoadI4" SkipInitLocals [| loadIntegerValue 4 |]
          StoreI32 = defineMemoryStore PrimitiveType.I4 4 "StoreI4" SkipInitLocals [| storeIntegerValue 4 |] }

    // TODO: Define option to specify that memories should be instantiated lazily.
    let addMemoryFields
        mem
        (initializer: byref<InstructionBlock>)
        (members: DefinedTypeMembers)
        memories
        exports
        metadata
        =
        let memories'' = Dictionary<_, FieldTok>()
        match memories with
        | ValueSome(memories': MemorySection) ->
            if memories'.Length > 1 then
                raise(NotSupportedException "Multiple memory instances are not yet supported by most implementations of WebAssembly")

            memories''.EnsureCapacity memories'.Length |> ignore

            for i = 0 to memories'.Length - 1 do
                let mfield =
                    members.DefineField (
                        DefinedField.Static (
                            MemberVisibility.CompilerControlled,
                            FieldAttributes.InitOnly,
                            Identifier.ofStr("memory#" + string i),
                            mem.Type
                        ),
                        ValueNone
                    )
                    |> ValidationResult.get

                memories''.[memories'.First + uint32 i] <- mfield.Token

            match exports with
            | ValueSome(exports': ModuleExports) ->
                let memGetterType = ReturnType.T mem.Type

                for struct(name, memi) in ModuleExports.memories exports' do
                    let getter =
                        DefinedMethod.Static (
                            MemberVisibility.Public,
                            MethodAttributes.SpecialName ||| MethodAttributes.HideBySig,
                            memGetterType,
                            MethodName.ofStr("get_" + name),
                            ImmutableArray.Empty,
                            Parameter.emptyList
                        )

                    let getter' =
                        MethodBody.ofSeq [|
                            ldsfld memories''.[memi]
                            ret
                        |]

                    members.DefineProperty (
                        Identifier.ofStr name,
                        ValueSome(getter :> DefinedMethod, ValueSome getter', ValueNone),
                        ValueNone,
                        List.empty,
                        ValueNone
                    )
                    |> ValidationResult.get
                    |> ignore
            | ValueNone -> ()

            initializer <-
                InstructionBlock.ofList [
                    for i = 0 to memories'.Length - 1 do
                        let i' = memories'.First + uint32 i
                        let (MemType size) = memories'.[i'].Type
                        let max =
                            match size.Max with
                            | ValueSome max' -> max'
                            | ValueNone -> 32768u // Maximum number of pages before length (as an int32) overflows.

                        ldc_i4(int32 size.Min)
                        ldc_i4(int32 max)
                        Cil.Instructions.Newobj.ofMethod mem.Constructor.Token
                        stsfld memories''.[i']
                ]
        | ValueNone -> ()
        memories''

    type HelperMethods =
        { Memory:
            {| Grow: MethodTok<DefinedType, DefinedMethod>
               LoadI32: MethodTok<DefinedType, DefinedMethod>
               StoreI32: MethodTok<DefinedType, DefinedMethod> |} }

    let addHelperMethods mscorlib (mem: MemoryType) (members: DefinedTypeMembers) =
        let inline addMemoryHelper rtype name ptypes parameters body =
            members.DefineMethod (
                DefinedMethod.Static (
                    MemberVisibility.CompilerControlled,
                    MethodAttributes.None,
                    rtype,
                    MethodName.ofStr name,
                    ImmutableArray.CreateRange(Seq.map ParameterType.T ptypes),
                    parameters
                ),
                ValueSome(MethodBody.ofSeq body),
                ValueNone
            )
            |> ValidationResult.get

        let addMemoryLoader =
            let parameters i _ =
                match i with
                | 0 -> "offset"
                | 1 -> "memarg.alignment"
                | 2 -> "memarg.offset"
                | _ -> "memory"
                |> Identifier.ofStr
                |> Parameter.named

            fun ltype name (helper: MethodTok<DefinedType, DefinedMethod>) ->
                addMemoryHelper
                    (ReturnType.T ltype)
                    name
                    [| PrimitiveType.I4; PrimitiveType.U4; PrimitiveType.I4; mem.Type |]
                    parameters
                    [|
                        ldarg_3
                        ldarg_1
                        ldarg_2
                        ldarg_0
                        add_ovf
                        Cil.Instructions.call helper.Token
                        ret
                    |]

        { Memory =
            {| Grow =
                addMemoryHelper
                    (ReturnType.T PrimitiveType.I4)
                    "memory.grow"
                    [| PrimitiveType.I4; mem.Type |]
                    Parameter.emptyList
                    [
                        ldarg_1
                        ldarg_0
                        Cil.Instructions.call mem.Grow
                        ret
                    ]
               LoadI32 = addMemoryLoader PrimitiveType.I4 "i32.load" mem.LoadI32
               StoreI32 =
                addMemoryHelper
                    ReturnType.Void'
                    "i32.store"
                    [| PrimitiveType.I4; PrimitiveType.I4; PrimitiveType.U4; PrimitiveType.I4; mem.Type |]
                    (fun i _ ->
                        match i with
                        | 0 -> "offset"
                        | 1 -> "value"
                        | 2 -> "memarg.alignment"
                        | 3 -> "memarg.offset"
                        | _ -> "memory"
                        |> Identifier.ofStr
                        |> Parameter.named)
                    [|
                        ldarg_s 4uy
                        ldarg_2
                        ldarg_0
                        ldarg_3
                        add_ovf
                        ldarg_1
                        Cil.Instructions.call mem.StoreI32.Token
                        ret
                    |] |}}

    type ReferencedImports =
        { Functions: Dictionary<Index<IndexKinds.Func>, MethodTok> }

    type MethodBodyBuilder =
        { Instructions: ImmutableArray<Cil.Instruction>.Builder
          Blocks: ImmutableArray<Cil.InstructionBlock>.Builder
          // TODO: Make custom label list class.
          Labels: List<Cil.Label ref>
          LabelIndices: Stack<int32>
          IfLabelFixups: Stack<Cil.Label ref>
          Helpers: HelperMethods
          Imports: ReferencedImports
          MainMemoryIndex: Index<IndexKinds.Mem>
          MemoryLookup: Dictionary<Index<IndexKinds.Mem>, FieldTok> }

    let translateMethodBody
        funcParamCount
        body
        { Instructions = instrs
          Blocks = blocks
          Labels = labels
          LabelIndices = lindices
          IfLabelFixups = ifLabelFixups
          Helpers = helpers
          Imports = imports
          MainMemoryIndex = mainMemIndex
          MemoryLookup = memories }
        finish
        =
        instrs.Clear()
        blocks.Clear()
        labels.Clear() // NOTE: This assumes that, within a block, labels are numbered sequentially
        lindices.Clear()
        ifLabelFixups.Clear()

        let inline emit op = instrs.Add op

        let inline emitVarOp argi loci (Index i) =
            let i' = Checked.uint16 i
            if i < funcParamCount
            then argi(Checked.uint16 i)
            else loci(LocalVarIndex.locali i')
            |> emit

        let inline emitMemArg { MemArg.Alignment = MemArgAlignment align; Offset = offset } =
            emit(ldc_i4(int32 align))
            emit(ldc_i4(int32 offset))
            emit(ldsfld memories.[mainMemIndex])

        let inline emitShortBranchRef opcode target =
            emit(Instruction.branchingRef opcode (StackBehavior.PopOrPush -1y) BranchKind.Short target)

        let inline pushLabelRef() =
            let i, l = labels.Count, ref Unchecked.defaultof<_>
            lindices.Push i
            labels.Add l
            l

        let inline insertLabelRef l =
            let struct(l', bl) = InstructionBlock.label InstructionBlock.empty
            blocks.Add bl
            l.contents <- l'

        let inline branchToLabel opcode str (Index index: Index<IndexKinds.Label>) =
            Instruction.branchingRef opcode str BranchKind.Long labels.[labels.Count - 1 - Checked.int32 index] |> emit

        let inline commitInstructionList() =
            blocks.Add(InstructionBlock.ofBlock(instrs.ToImmutable()))
            instrs.Clear()

        let inline emitComparisonAndEqualityOperator opcode =
            let ttrue, tfalse = ref Unchecked.defaultof<_>, ref Unchecked.defaultof<_>
            emitShortBranchRef opcode ttrue
            commitInstructionList()
            ImmutableArray.Create (
                ldc_i4_0,
                Instruction.branchingRef Opcode.Br_s (StackBehavior.PopOrPush -1y) BranchKind.Short tfalse
            )
            |> InstructionBlock.ofBlock
            |> blocks.Add
            let struct(ttrue', true') = InstructionBlock.singleton ldc_i4_1 |> InstructionBlock.label
            blocks.Add(true')
            ttrue.contents <- ttrue'
            insertLabelRef tfalse

        for instr in Expr.toBlock body do
            match instr with
            // Control Instructions
            | { Opcode = 0x01uy; Arguments = InstructionArguments.Nothing } -> emit Cil.Instructions.nop // nop
            | { Opcode = 0x03uy; Arguments = InstructionArguments.BlockType _ } -> // loop
                commitInstructionList()
                pushLabelRef() |> insertLabelRef
            | { Opcode = 0x04uy; Arguments = InstructionArguments.BlockType _ } -> // if
                let l = pushLabelRef()
                emit(Instruction.branchingRef Opcode.Brfalse (StackBehavior.PopOrPush -1y) BranchKind.Long l)
                ifLabelFixups.Push l
            | { Opcode = 5uy | 0x0Buy; Arguments = InstructionArguments.Nothing } -> // end, else
                if ifLabelFixups.Count > 0 then
                    commitInstructionList()
                    ifLabelFixups.Pop() |> insertLabelRef
            | { Opcode = 0x10uy; Arguments = InstructionArguments.FuncIndex i } ->
                match imports.Functions.TryGetValue i with
                | true, f -> emit(Cil.Instructions.call f)
                | false, _ ->
                    failwith "TODO: Make creation of method bodies lazy, since calls to methods defined in the current module may be messed up (later indices)"
            | { Opcode = 0x0Cuy; Arguments = InstructionArguments.LabelIndex i } ->
                branchToLabel Opcode.Br (StackBehavior.PopOrPush 0y) i

            // Memory Instructions
            | { Opcode = 0x1Auy; Arguments = InstructionArguments.Nothing } -> emit pop // drop
            | { Opcode = 0x20uy; Arguments = InstructionArguments.LocalIndex i } -> // local.get
                emitVarOp Shortened.ldarg Shortened.ldloc i
            | { Opcode = 0x21uy; Arguments = InstructionArguments.LocalIndex i } -> // local.set
                emitVarOp Shortened.starg Shortened.stloc i
            | { Opcode = 0x28uy; Arguments = InstructionArguments.MemArg arg } -> // i32.load
                emitMemArg arg
                emit(Cil.Instructions.call helpers.Memory.LoadI32.Token)
            | { Opcode = 0x36uy; Arguments = InstructionArguments.MemArg arg } -> // i32.store
                emitMemArg arg
                emit(Cil.Instructions.call helpers.Memory.StoreI32.Token)
            | { Opcode = 0x40uy; Arguments = InstructionArguments.MemoryIndex i } -> // memory.grow
                emit(ldsfld memories.[i])
                emit(Cil.Instructions.call helpers.Memory.Grow.Token)

            // Numeric Instructions
            | { Opcode = 0x41uy; Arguments = InstructionArguments.I32 n } -> emit (Shortened.ldc_i4 n) // i32.const
            | { Opcode = 0x42uy; Arguments = InstructionArguments.I64 n } -> emit (ldc_i8 n) // i64.const
            | { Opcode = 0x45uy; Arguments = InstructionArguments.Nothing } -> // i32.eqz
                emit ldc_i4_0
                emit ceq
            | { Opcode = 0x46uy; Arguments = InstructionArguments.Nothing } -> emit ceq // i32.eq
            | { Opcode = 0x47uy; Arguments = InstructionArguments.Nothing } -> // i32.ne
                emit ceq
                emit ldc_i4_0
                emit ceq
            | { Opcode = 0x48uy; Arguments = InstructionArguments.Nothing } -> emit clt // i32.lt_s
            | { Opcode = 0x49uy; Arguments = InstructionArguments.Nothing } -> emit clt_un // i32.lt_u
            | { Opcode = 0x4Auy; Arguments = InstructionArguments.Nothing } -> emit cgt // i32.gt_s
            | { Opcode = 0x4Buy; Arguments = InstructionArguments.Nothing } -> emit cgt_un // i32.gt_u
            // CIL does not have any comparison opcodes for both greater than/less than and equality at the same time, so
            // branching instructions need to be used instead.
            | { Opcode = 0x4Cuy; Arguments = InstructionArguments.Nothing } -> emitComparisonAndEqualityOperator Opcode.Ble_s // i32.le_s
            | { Opcode = 0x4Duy; Arguments = InstructionArguments.Nothing } -> emitComparisonAndEqualityOperator Opcode.Ble_un_s // i32.le_u
            | { Opcode = 0x4Euy; Arguments = InstructionArguments.Nothing } -> emitComparisonAndEqualityOperator Opcode.Bge_s // i32.ge_s
            | { Opcode = 0x4Fuy; Arguments = InstructionArguments.Nothing } -> emitComparisonAndEqualityOperator Opcode.Bge_un_s // i32.ge_u

            | { Opcode = 0x52uy; Arguments = InstructionArguments.Nothing } ->
                emit ceq
                emit ldc_i4_0
                emit ceq
            | { Opcode = 0x6Auy | 0x7Cuy; Arguments = InstructionArguments.Nothing } -> emit add // i32.add, i64.add
            | { Opcode = 0x6Buy | 0x7Duy; Arguments = InstructionArguments.Nothing } -> emit sub // i32.sub, i64.sub
            | { Opcode = 0x6Cuy | 0x7Euy; Arguments = InstructionArguments.Nothing } -> emit mul // i32.mul, i64.mul
            | { Opcode = 0x6Duy | 0x7Fuy; Arguments = InstructionArguments.Nothing } -> emit div // i32.div_s, i64.div_s
            | { Opcode = 0x6Euy | 0x80uy; Arguments = InstructionArguments.Nothing } -> emit div_un // i32.div_u, i64.div_u
            | { Opcode = 0x71uy | 0x83uy; Arguments = InstructionArguments.Nothing } -> emit ``and`` // i32.and, i64.and
            | { Opcode = 0x74uy; Arguments = InstructionArguments.Nothing } -> emit shl // i32.shl
            | { Opcode = 0x76uy; Arguments = InstructionArguments.Nothing } -> emit shr_un // i32.shr_u
            | _ -> failwithf "TODO: Error for cannot translate unknown opcode 0x%02X" instr.Opcode

        if instrs.Count > 0 then commitInstructionList()
        if ifLabelFixups.Count > 0 then invalidOp "Missing label for some if instructions"

        finish blocks
        blocks.ToImmutable()

    let addTranslatedGlobals (members: DefinedTypeMembers) (globals: GlobalSection voption) exports =
        match globals with
        | ValueSome globals' ->
            let lookup = Dictionary globals'.Length
            let initializers = List globals'.Length

            let getGlobalName =
                match exports with
                | ValueSome exports' ->
                    ModuleExports.tryGetGlobal exports' >> ValueOption.map (fun export -> Identifier.ofStr export.Name)
                | ValueNone -> fun _ -> ValueNone

            for i = 0 to globals'.Length - 1 do
                let i' = globals'.First + uint32 i
                let gl = globals'.[i']

                let mutability, gtype =
                    match gl.Type with
                    | GlobalType.Const gt -> FieldAttributes.InitOnly, gt
                    | GlobalType.Var gt -> FieldAttributes.None, gt

                let gtype' = getValueType gtype
                let name = getGlobalName i'

                let gl' =
                    members.DefineField (
                        DefinedField.Static (
                            MemberVisibility.CompilerControlled,
                            mutability,
                            Identifier.ofStr ("global#" + string i),
                            gtype'
                        ),
                        ValueNone
                    )
                    |> ValidationResult.get

                lookup.Add(i', gl'.Token)

                let initializer builder =
                    let body =
                        fun (blocks: ImmutableArray<_>.Builder) ->
                            ImmutableArray.Create(stsfld gl'.Token, ret)
                            |> InstructionBlock.ofBlock
                            |> blocks.Add
                        |> translateMethodBody 0u gl.Expression builder
                        |> MethodBody.create InitLocals ValueNone LocalVariables.Null

                    members.DefineMethod (
                        DefinedMethod.Static (
                            MemberVisibility.CompilerControlled,
                            MethodAttributes.None,
                            ReturnType.Void',
                            MethodName.ofStr("initglobal#" + string i),
                            ImmutableArray.Empty,
                            Parameter.emptyList
                        ),
                        ValueSome body,
                        ValueNone
                    )
                    |> ValidationResult.get

                initializers.Add initializer

                match name with
                | ValueSome name' ->
                    let getter =
                        let getter' =
                            DefinedMethod.Static (
                                MemberVisibility.Public,
                                MethodAttributes.HideBySig,
                                ReturnType.T gtype',
                                MethodName.ofStr("get_" + string name'),
                                ImmutableArray.Empty,
                                Parameter.emptyList
                            )

                        ValueSome(getter' :> DefinedMethod, ValueSome(MethodBody.ofSeq [| ldsfld gl'.Token; ret |]), ValueNone)

                    let setter =
                        if mutability = FieldAttributes.InitOnly
                        then ValueNone
                        else
                            let setter' =
                                DefinedMethod.Static (
                                    MemberVisibility.Public,
                                    MethodAttributes.HideBySig,
                                    ReturnType.Void',
                                    MethodName.ofStr("set_" + string name'),
                                    ImmutableArray.Create(ParameterType.T gtype'),
                                    Parameter.emptyList
                                )

                            let body = MethodBody.ofSeq [| ldarg_1; stsfld gl'.Token; ret |]

                            ValueSome(setter' :> DefinedMethod, ValueSome body, ValueNone)

                    members.DefineProperty(name', getter, setter, List.empty, ValueNone)
                    |> ValidationResult.get
                    |> ignore
                | ValueNone -> ()

            struct(lookup, initializers)
        | ValueNone -> struct(Dictionary 0, List 0)

    let addTranslatedFunctions methodBodyBuilder (members: DefinedTypeMembers) sections exports =
        match sections with
        | { TypeSection = ValueSome types
            FunctionSection = ValueSome funcs
            CodeSection = ValueSome code }
            ->
            let methods = Dictionary<Index<IndexKinds.Func>, MethodTok> funcs.Length

            // Shared to avoid extra allocations.
            let locals = ImmutableArray.CreateBuilder()
            let endTranslatedFunction (blocks: ImmutableArray<_>.Builder) = blocks.Add(InstructionBlock.singleton ret)

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
                        | ValueNone -> MethodDefFlags.CompilerControlled, MethodName.ofStr ("func#" + string i)
                    DefinedMethod (
                        MethodImplFlags.IL,
                        MethodDefFlags.Static ||| MethodDefFlags.HideBySig ||| visibility,
                        FSharpIL.Metadata.Signatures.MethodThis.NoThis,
                        getReturnType funct.Results,
                        name,
                        getParameterTypes funct.Parameters,
                        Parameter.emptyList
                    )

                // TODO: Use method dictionary when generating call instructions.
                methods.[i'] <-
                    let code' = &code.ItemRef(i)
                    let body =
                        translateMethodBody
                            (Checked.uint32 funct.Parameters.Length)
                            code'.Body
                            methodBodyBuilder
                            endTranslatedFunction
                        // WebAssembly specification states that variables are all zeroed out before use.
                        |> MethodBody.create InitLocals ValueNone (getMethodLocals locals code'.Locals)

                    let token = members.DefineMethod(func', ValueSome body, ValueNone) |> ValidationResult.get
                    token.Token

            methods
        | _ -> failwith "TODO: How to deal with missing sections?"

    let getOwningModule { Import.Module = mname } (modules: Dictionary<_, _>) (metadata: CliModuleBuilder) =
        match modules.TryGetValue mname with
        | true, existing -> existing
        | false, _ ->
            let assem =
                { ReferencedAssembly.Name = FileName.ofStr mname
                  Version = AssemblyVersion.Zero
                  PublicKeyOrToken = NoPublicKeyOrToken
                  Culture = ValueNone
                  HashValue = ImmutableArray.Empty }

            metadata.ReferenceAssembly assem

            let tref =
                { TypeReference.Flags = ValueNone
                  TypeName = Identifier.ofStr mname
                  TypeNamespace = ValueNone
                  ResolutionScope = TypeReferenceParent.Assembly assem }
                |> ReferencedType.Reference
                |> metadata.ReferenceType
                |> ValidationResult.get

            modules.Add(mname, tref)
            tref

    let addImportReferences sections (metadata: CliModuleBuilder) =
        match sections with
        | { KnownSections.ImportSection = ValueSome imports; TypeSection = ValueSome types } ->
            let modules = Dictionary()
            let references =
                { ReferencedImports.Functions = Dictionary() }

            // TODO: When parsing metadata, check that there are no duplicate export names

            for i = 0 to imports.Functions.Length - 1 do
                let i' = Index<_>(uint32 i)
                let func = imports.Functions.[i']
                let owner = getOwningModule func modules metadata
                let ftype = types.[func.Description]

                let func' =
                    ReferencedMethod.Static (
                        ExternalVisibility.Public,
                        getReturnType ftype.Results,
                        MethodName.ofStr func.Name,
                        getParameterTypes ftype.Parameters
                    )
                    |> owner.ReferenceMethod
                    |> ValidationResult.get

                references.Functions.Add(i', func'.Token)

            references
        | _ ->
            { ReferencedImports.Functions = Dictionary() }

    let toPE (ValidatedModule file) options = // TODO: Add option to set file to Exe if start function is defined.
        let sections = getKnownSections file
        let exports = ValueOption.map getModuleExports sections.ExportSection
        let extension = FileType.extension options.FileType

        // TODO: Figure out how to deterministically generate MVID
        let metadata = CliModuleBuilder(Identifier.ofStr(options.ModuleFileName + "." + extension))
        // TODO: Figure out how to allow usage of other core assemblies?
        let mscorlib = addCoreAssembly metadata
        do addAssemblyDefinition options metadata |> ignore

        let module' =
            { TypeDefinition.TypeName = Identifier.ofStr options.ModuleFileName
              TypeNamespace = ValueNone // TODO: If module name is separated by periods, make the first few things the namespace
              Flags = TypeDefFlags.Public ||| TypeDefFlags.Sealed ||| TypeDefFlags.Abstract
              Extends =
                ReferencedType.Reference mscorlib.Object.Type
                |> NamedType.ReferencedType 
                |> ClassExtends.Named
              EnclosingClass = ValueNone }
            |> DefinedType.Definition

        let members = metadata.DefineType(module', ValueNone) |> ValidationResult.get

        let imports = addImportReferences sections metadata

        let initializer = Array.zeroCreate 4
        let struct(globals, globalVariableInitializers) = addTranslatedGlobals members sections.GlobalSection exports
        let mem = addMemoryType mscorlib module' exports metadata
        let memories = addMemoryFields mem &initializer.[1] members sections.MemorySection exports metadata
        //let tables = &initializer.[2]
        initializer.[2] <- InstructionBlock.empty // TEMPORARY

        let helpers = addHelperMethods mscorlib mem members
        let methodBodyBuilder =
            { Instructions = ImmutableArray.CreateBuilder()
              Blocks = ImmutableArray.CreateBuilder()
              Labels = List()
              LabelIndices = Stack()
              IfLabelFixups = Stack()
              Helpers = helpers
              Imports = imports
              MainMemoryIndex =
                match sections.MemorySection with
                | ValueSome memories' -> memories'.First
                | ValueNone -> Index.Zero
              MemoryLookup = memories }

        let functions = addTranslatedFunctions methodBodyBuilder members sections exports

        initializer.[0] <- InstructionBlock.ofSeq [|
            for init in globalVariableInitializers do
                let init' = init methodBodyBuilder
                Cil.Instructions.call init'.Token
        |]

        initializer.[3] <- InstructionBlock.ofList [
            // TODO: In WASM parsing code, check that signature of start function is valid.
            match sections.StartSection with
            | ValueSome start -> Cil.Instructions.call functions.[start]
            | ValueNone -> ()

            ret
        ]

        members.DefineMethod (
            DefinedMethod.ClassConstructor,
            ValueSome(MethodBody.create InitLocals ValueNone LocalVariables.Null initializer),
            ValueNone
        )
        |> ValidationResult.get
        |> ignore

        setTargetFramework options mscorlib.TargetFrameworkAttribute.Constructor metadata

        // TODO: Add FSharpIL function to allow translation of CliModuleBuilder to section contents
        if options.HighEntropyVA then () //failwith "// TODO: Allow setting of ASLR flag in optional header."
        BuildPE.ofModuleBuilder FileCharacteristics.IsDll metadata
