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
                Cil.Instructions.``or``
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
                        let size = memories'.[i'].Type
                        let max =
                            match size.Max with
                            | ValueSome max' -> max'.Multiple
                            | ValueNone -> System.UInt16.MaxValue

                        ldc_i4(int32 size.Min.Multiple)
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

    let translateMethodBody
        locinit
        funcParamCount
        (instrs: ImmutableArray<Cil.Instruction>.Builder)
        (blocks: ImmutableArray<_>.Builder)
        // TODO: Make custom label list class.
        (labels: List<Cil.Label ref>)
        (lindices: Stack<int32>)

        (ifLabelFixups: Stack<Cil.Label ref>)
        (localTypesBuilder: ImmutableArray<_>.Builder)
        { Code.Locals = locals; Body = body }
        helpers
        mainMemIndex
        (memories: Dictionary<_, FieldTok>)
        =
        instrs.Clear()
        blocks.Clear()
        labels.Clear() // NOTE: This assumes that, within a block, labels are numbered sequentially
        lindices.Clear()
        ifLabelFixups.Clear()
        localTypesBuilder.Clear()

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

        for instr in Expr.toBlock body do
            match instr with
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
            | { Opcode = 0x0Cuy; Arguments = InstructionArguments.LabelIndex i } ->
                branchToLabel Opcode.Br (StackBehavior.PopOrPush 0y) i
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
            | { Opcode = 0x41uy; Arguments = InstructionArguments.I32 n } -> emit (Shortened.ldc_i4 n) // i32.const
            | { Opcode = 0x42uy; Arguments = InstructionArguments.I64 n } -> emit (ldc_i8 n) // i64.const
            | { Opcode = 0x52uy; Arguments = InstructionArguments.Nothing } ->
                emit ceq
                emit ldc_i4_0
                emit ceq
            | { Opcode = 0x6Auy | 0x7Cuy; Arguments = InstructionArguments.Nothing } -> emit add // i32.add, i64.add
            | { Opcode = 0x6Buy | 0x7Duy; Arguments = InstructionArguments.Nothing } -> emit sub // i32.sub, i64.sub
            | { Opcode = 0x6Cuy | 0x7Euy; Arguments = InstructionArguments.Nothing } -> emit mul // i32.mul, i64.mul
            | _ -> failwithf "TODO: Error for cannot translate unknown opcode 0x%02X" instr.Opcode

        if instrs.Count > 0 then commitInstructionList()
        if ifLabelFixups.Count > 0 then invalidOp "Missing label for some if instructions"

        blocks.Add(InstructionBlock.singleton ret)

        blocks.ToImmutable()
        |> MethodBody.create locinit ValueNone (getMethodLocals localTypesBuilder locals)
        |> ValueSome

    let addTranslatedFunctions helpers memories (members: DefinedTypeMembers) sections exports =
        match sections with
        | { TypeSection = ValueSome types
            FunctionSection = ValueSome funcs
            CodeSection = ValueSome code }
            ->
            let methods = Dictionary<Index<IndexKinds.Func>, MethodTok> funcs.Length

            // Shared to avoid extra allocations.
            let locals = ImmutableArray.CreateBuilder()
            let instrs = ImmutableArray.CreateBuilder()
            let blocks = ImmutableArray.CreateBuilder()
            let labels = List<Cil.Label ref>()
            let lindices = Stack<int32>()
            let ifLabelFixups = Stack<Cil.Label ref>()

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
                    let body =
                        translateMethodBody
                            InitLocals // WebAssembly specification states that variables are all zeroed out before use.
                            (Checked.uint32 funct.Parameters.Length)
                            instrs
                            blocks
                            labels
                            lindices
                            ifLabelFixups
                            locals
                            code.[i]
                            helpers
                            (match sections.MemorySection with
                            | ValueSome memories' -> memories'.First
                            | ValueNone -> Index.Zero)
                            memories

                    let token = members.DefineMethod(func', body, ValueNone) |> ValidationResult.get
                    token.Token

            methods
        | _ -> failwith "TODO: How to deal with missing sections?"

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

        let initializer = Array.zeroCreate 3
        let mem = addMemoryType mscorlib module' exports metadata
        let memories = addMemoryFields mem &initializer.[0] members sections.MemorySection exports metadata
        //let tables = &initializer.[1]
        initializer.[1] <- InstructionBlock.empty // TEMPORARY

        let helpers = addHelperMethods mscorlib mem members
        let functions = addTranslatedFunctions helpers memories members sections exports

        initializer.[2] <- InstructionBlock.ofList [
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
