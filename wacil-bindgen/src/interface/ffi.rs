use crate::interface::types::{ReturnType, Type};
use crate::interface::Name;

#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct Parameter<'a> {
    pub name: Name,
    pub parameters: &'a [Type<'a>],
}

#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct FfiMethod<'a> {
    pub name: Name,
    pub return_type: ReturnType<'a>,
    pub parameters: &'a [Parameter<'a>],
}
