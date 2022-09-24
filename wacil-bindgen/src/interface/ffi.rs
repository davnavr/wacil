use crate::interface::types::{ReturnType, Type};
use crate::interface::Name;

#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct Parameter {
    pub name: Name,
    pub parameters: &'static [Type<'static>],
}

#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct Method {
    pub name: Name,
    pub is_static: bool,
    pub return_type: ReturnType<'static>,
    pub parameters: &'static [Parameter],
}
