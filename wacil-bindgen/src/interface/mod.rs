//! Low-level module for generating the information needed to pass to the `wacil_backend`.

pub mod types;

mod ffi;
mod import;
mod name;

pub use ffi::FfiMethod;
pub use import::{ClassImport, ClassImportDescriptor};
pub use name::{Name, Namespace, TypeName};

#[derive(Clone, Debug)]
#[non_exhaustive]
pub struct Interface {
    pub imported_classes: &'static [ClassImport],
}

impl Interface {
    pub const fn new() -> Self {
        Self { imported_classes: &[] }
    }
}

impl Default for Interface {
    fn default() -> Self {
        Self::new()
    }
}
