namespace Wacil.Compiler.Emit

[<NoComparison; StructuralEquality>]
type Options =
    { Namespace: string }

    static member Default =
        { Namespace = System.String.Empty }
