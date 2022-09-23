use crate::interface::{FfiMethod, TypeName};

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct MethodImport(&'static FfiMethod);

/// Describes that a CLR class is used in Rust code.
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct ClassImport {
    pub name: TypeName,
    pub methods: &'static [MethodImport],
}

impl ClassImport {
    pub const fn new(name: TypeName) -> Self {
        Self { name, methods: &[] }
    }
}

pub trait ClassImportDescriptor {
    const DESCRIPTOR: &'static ClassImport;
}
