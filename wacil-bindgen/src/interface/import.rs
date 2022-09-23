use crate::interface::{Method, TypeName};

/// Describes a CLR class that is used in Rust code.
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct ClassImport {
    pub name: TypeName,
    pub methods: &'static [Method],
}

impl ClassImport {
    pub const fn new(name: TypeName) -> Self {
        Self { name, methods: &[] }
    }
}

pub trait ClassImportDescriptor {
    const DESCRIPTOR: &'static ClassImport;
}
