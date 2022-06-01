namespace Wacil.Runtime {
    using System;

    public sealed class UnreachableException : Exception {
        public UnreachableException() : base("Encountered unreachable point in code") {}
    }
}
