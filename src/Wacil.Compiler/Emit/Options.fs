namespace Wacil.Compiler.Emit

open System.Runtime.CompilerServices

[<NoComparison; StructuralEquality>]
type Options =
    { Name: string
      ///// <summary>If set, indicates that the module name should be obtained from the custom <c>name</c> section.</summary>
      //InferName: bool
      /// Indicates the name of the namespace containing the generated class.
      Namespace: string }
