//! Provides runtime support functions provided by `Wacil.Runtime.Rust` for compiled Rust programs.

extern "C" {
    /// Indicates that the CLR object corresponding to the given index is no longer used by Rust code, allowing the index to be
    /// reused for other object references.
    pub fn wacil_rt_object_table_drop(index: isize);
}
