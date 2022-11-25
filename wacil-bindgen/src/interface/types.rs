//! Describes the CLR type system.

use core::fmt::{Display, Formatter};

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum Type<'a> {
    Named {
        name: crate::interface::TypeName,
        arguments: &'a [Type<'a>],
    },
    Byte,
    SByte,
    Int16,
    UInt16,
    Int32,
    UInt32,
    Int64,
    UInt64,
    Single,
    Double,
    Object,
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        use core::fmt::Write;
        match self {
            Self::Named { name, arguments } => {
                Display::fmt(name, f)?;
                if !arguments.is_empty() {
                    f.write_char('<')?;
                    arguments.iter().enumerate().try_for_each(|(index, ty)| {
                        if index > 0 {
                            f.write_str(", ")?;
                        }
                        Display::fmt(ty, f)
                    })?;
                    f.write_char('>')?;
                }
                Ok(())
            }
            Self::Byte => f.write_str("byte"),
            Self::SByte => f.write_str("sbyte"),
            Self::Int16 => f.write_str("short"),
            Self::UInt16 => f.write_str("ushort"),
            Self::Int32 => f.write_str("int"),
            Self::UInt32 => f.write_str("uint"),
            Self::Int64 => f.write_str("long"),
            Self::UInt64 => f.write_str("ulong"),
            Self::Single => f.write_str("float"),
            Self::Double => f.write_str("double"),
            Self::Object => f.write_str("object"),
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum ReturnType<'a> {
    Type(Type<'a>),
    Void,
}

impl Display for ReturnType<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::Type(ty) => Display::fmt(ty, f),
            Self::Void => f.write_str("void"),
        }
    }
}
