namespace Wacil.Compiler.Emit

open AsmResolver.PE.DotNet.Cil

open AsmResolver.DotNet
open AsmResolver.DotNet.Code.Cil

[<AutoOpen>]
module internal CilInstructionExtensions =
    type CilInstruction with
        static member CreateLdloc index =
            match index with
            | 0us -> CilInstruction CilOpCodes.Ldloc_0
            | 1us -> CilInstruction CilOpCodes.Ldloc_1
            | 2us -> CilInstruction CilOpCodes.Ldloc_2
            | 3us -> CilInstruction CilOpCodes.Ldloc_3
            | _ when index <= 255us -> CilInstruction(CilOpCodes.Ldloc_S, uint8 index)
            | _ -> CilInstruction(CilOpCodes.Ldloc, index)

        static member CreateLdarg index =
            match index with
            | 0us -> CilInstruction CilOpCodes.Ldarg_0
            | 1us -> CilInstruction CilOpCodes.Ldarg_1
            | 2us -> CilInstruction CilOpCodes.Ldarg_2
            | 3us -> CilInstruction CilOpCodes.Ldarg_3
            | _ when index <= 255us -> CilInstruction(CilOpCodes.Ldarg_S, uint8 index)
            | _ -> CilInstruction(CilOpCodes.Ldarg, index)

        static member CreateStloc index =
            match index with
            | 0us -> CilInstruction CilOpCodes.Stloc_0
            | 1us -> CilInstruction CilOpCodes.Stloc_1
            | 2us -> CilInstruction CilOpCodes.Stloc_2
            | 3us -> CilInstruction CilOpCodes.Stloc_3
            | _ when index <= 255us -> CilInstruction(CilOpCodes.Stloc_S, uint8 index)
            | _ -> CilInstruction(CilOpCodes.Stloc, index)

        static member CreateStarg index =
            if index <= 255us
            then CilInstruction(CilOpCodes.Starg_S, uint8 index)
            else CilInstruction(CilOpCodes.Starg, index)

[<RequireQualifiedAccess>]
module internal CilHelpers =
    type Instructions = CilInstructionCollection

    let emitObjectCtorCall (syslib: SystemLibrary.References) (il: Instructions) =
        il.Add(CilInstruction CilOpCodes.Ldarg_0)
        il.Add(CilInstruction(CilOpCodes.Call, syslib.Object.Constructor))

    /// Generates code to store a parameter into an instance field, performing a null check.
    let emitArgumentStoreWithNullCheck
        (syslib: SystemLibrary.References)
        parameter
        (nullArgumentName: string)
        (field: FieldDefinition)
        (il: Instructions)
        =
        let store = CilInstruction(CilOpCodes.Stfld, field)
        il.Add(CilInstruction CilOpCodes.Ldarg_0)
        il.Add(CilInstruction.CreateLdarg parameter)
        il.Add(CilInstruction CilOpCodes.Dup) // Duplicate the object reference to store
        il.Add(CilInstruction(CilOpCodes.Brtrue_S, CilInstructionLabel store))
        il.Add(CilInstruction(CilOpCodes.Ldstr, nullArgumentName))
        il.Add(CilInstruction(CilOpCodes.Newobj, syslib.ArgumentNullExceptionConstructor))
        il.Add(CilInstruction CilOpCodes.Throw)
        il.Add store
