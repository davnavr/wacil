use crate::interface::types::{ReturnType, Type};
use crate::interface::Name;

#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct Parameter {
    pub name: Name,
    pub parameters: &'static [Type],
}

#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct FfiMethod {
    pub name: Name,
    pub return_type: ReturnType,
    pub parameters: &'static [Parameter],
}
