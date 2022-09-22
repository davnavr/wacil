use crate::interface::{FfiMethod, TypeName};

#[derive(Clone, Copy, Debug)]
#[repr(transparent)]
pub struct MethodImport<'a>(&'a FfiMethod<'a>);

/// Describes that a .NET class is used in Rust code.
#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct ClassImport<'a> {
    pub name: TypeName<'a>,
    pub methods: &'a [MethodImport<'a>],
}

impl<'a> ClassImport<'a> {
    pub fn new(name: TypeName<'a>) -> Self {
        Self {
            name,
            methods: Default::default(),
        }
    }
}
