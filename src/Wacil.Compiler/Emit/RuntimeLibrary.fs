/// <summary>Module for generating references to classes in the <c>Wacil.Runtime</c> assembly.</summary>
[<RequireQualifiedAccess>]
module internal Wacil.Compiler.Emit.RuntimeLibrary

open AsmResolver.DotNet
open AsmResolver.DotNet.Signatures.Types

/// <summary>Represents an instantiation of the <c>Wacil.Runtime.Table</c> class.</summary>
[<NoComparison; NoEquality>]
type TableClass =
    { Instantiation: GenericInstanceTypeSignature
      Specification: TypeSpecification
      Constructor: IMethodDefOrRef
      Get: IMethodDefOrRef }

/// <summary>Represents the references to the runtime library (<c>Wacil.Runtime.dll</c>).</summary>
[<NoComparison; NoEquality>]
type References =
    { UnreachableExceptionConstructor: IMethodDefOrRef
      Memory: ITypeDefOrRef
      MemorySignature: TypeSignature
      MemoryConstructor: IMethodDefOrRef
      MemoryI32Load: IMethodDefOrRef
      MemoryI32Store: IMethodDefOrRef
      MemoryGrow: IMethodDefOrRef
      MemoryWriteArray: IMethodDefOrRef
      Table: ITypeDefOrRef
      /// <summary>Instantiates the <c>Wacil.Runtime.Table</c> class for a given element type.</summary>
      InstantiatedTable: RefType -> TableClass }
