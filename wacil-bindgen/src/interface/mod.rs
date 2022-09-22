//! Low-level module for generating the information needed to pass to the `wacil_backend`.

pub mod types;

mod ffi;
mod import;
mod name;

pub use ffi::FfiMethod;
pub use import::ClassImport;
pub use name::{Name, Namespace, TypeName};

#[derive(Clone, Debug)]
pub struct Interface<'a> {
    pub imported_classes: &'a [ClassImport<'a>],
}

impl<'a> Interface<'a> {
    pub const fn new() -> Self {
        Self { imported_classes: &[] }
    }
}

impl Default for Interface<'_> {
    fn default() -> Self {
        Self::new()
    }
}
