//! Describes the CLR type system.

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Named(crate::interface::TypeName),
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
pub enum ReturnType {
    Type(Type),
    Void,
}
