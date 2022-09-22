//! Describes the .NET type system.

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Type<'a> {
    Named(crate::interface::TypeName<'a>),
    Byte,
    SByte,
    Int16,
    UInt16,
    Int32,
    UInt32,
    Int64,
    UInt64,
    Object,
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ReturnType<'a> {
    Type(Type<'a>),
    Void,
}
