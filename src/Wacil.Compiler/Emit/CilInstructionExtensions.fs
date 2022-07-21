[<AutoOpen>]
module internal Wacil.Compiler.Emit.CilInstructionExtensions

open AsmResolver.PE.DotNet.Cil

type CilInstruction with
    static member CreateLdloc index =
        match index with
        | 0 -> CilInstruction CilOpCodes.Ldloc_0
        | 1 -> CilInstruction CilOpCodes.Ldloc_1
        | 2 -> CilInstruction CilOpCodes.Ldloc_2
        | 3 -> CilInstruction CilOpCodes.Ldloc_3
        | _ when index < 0 -> raise(System.ArgumentOutOfRangeException(nameof index))
        | _ when index <= 255 -> CilInstruction(CilOpCodes.Ldloc_S, uint8 index)
        | _ -> CilInstruction(CilOpCodes.Ldloc, Checked.uint16 index)

    static member CreateLdarg index =
        match index with
        | 0 -> CilInstruction CilOpCodes.Ldarg_0
        | 1 -> CilInstruction CilOpCodes.Ldarg_1
        | 2 -> CilInstruction CilOpCodes.Ldarg_2
        | 3 -> CilInstruction CilOpCodes.Ldarg_3
        | _ when index < 0 -> raise(System.ArgumentOutOfRangeException(nameof index))
        | _ when index <= 255 -> CilInstruction(CilOpCodes.Ldarg_S, uint8 index)
        | _ -> CilInstruction(CilOpCodes.Ldarg, Checked.uint16 index)

    static member CreateStloc index =
        match index with
        | 0 -> CilInstruction CilOpCodes.Stloc_0
        | 1 -> CilInstruction CilOpCodes.Stloc_1
        | 2 -> CilInstruction CilOpCodes.Stloc_2
        | 3 -> CilInstruction CilOpCodes.Stloc_3
        | _ when index < 0 -> raise(System.ArgumentOutOfRangeException(nameof index))
        | _ when index <= 255 -> CilInstruction(CilOpCodes.Stloc_S, uint8 index)
        | _ -> CilInstruction(CilOpCodes.Stloc, Checked.uint16 index)

    static member CreateStarg index =
        match index with
        | _ when index < 0 -> raise(System.ArgumentOutOfRangeException(nameof index))
        | _ when index <= 255 -> CilInstruction(CilOpCodes.Starg_S, uint8 index)
        | _ -> CilInstruction(CilOpCodes.Starg, Checked.uint16 index)
