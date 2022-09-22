//! Module for manipulation of names.

use core::fmt::{Debug, Display, Formatter, Write};

/// Represents the name of a .NET class or member.
pub type Name = &'static str;

/// Represents a .NET namespace.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(transparent)]
pub struct Namespace(&'static [Name]);

impl Namespace {
    /// The empty namespace.
    pub const GLOBAL: Self = Self(&[]);

    pub const fn new(names: &'static [Name]) -> Self {
        Self(names)
    }

    pub const fn names(self) -> &'static [Name] {
        self.0
    }
}

impl Default for Namespace {
    fn default() -> Self {
        Self::GLOBAL
    }
}

impl Display for Namespace {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        self.names().iter().enumerate().try_for_each(|(index, name)| {
            if index > 0 {
                f.write_char('.')?;
            }

            Display::fmt(name, f)
        })
    }
}

/// Represents the name of a .NET type.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[non_exhaustive]
pub struct TypeName {
    pub namespace: Namespace,
    //parent_types: &'static [str],
    pub name: Name,
}

impl TypeName {
    pub const fn new(namespace: Namespace, name: Name) -> Self {
        Self { namespace, name }
    }
}
