//! Module for manipulation of names.

use core::fmt::{Debug, Display, Formatter, Write};

/// Represents the name of a .NET class or member.
#[derive(Clone, Copy, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct Name(&'static str);

impl Name {
    /// Creates a member name without any validation checks.
    ///
    /// # Safety
    ///
    /// Callers must ensure that the name does not contain any periods (`.`) and is not empty.
    pub const unsafe fn new_unchecked(name: &'static str) -> Self {
        Self(name)
    }

    /// Creates a member name.
    pub fn new(name: &'static str) -> Option<Self> {
        if name.is_empty() || name.contains('.') {
            None
        } else {
            Some(unsafe {
                // Safety: Validation checks performed above
                Self::new_unchecked(name)
            })
        }
    }

    pub fn as_str(self) -> &'static str {
        self.0
    }
}

impl Debug for Name {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        Debug::fmt(self.as_str(), f)
    }
}

impl Display for Name {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        Display::fmt(self.as_str(), f)
    }
}

impl core::ops::Deref for Name {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        self.as_str()
    }
}

/// Represents a .NET namespace.
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
#[repr(transparent)]
pub struct Namespace<'a>(&'a [Name]);

impl<'a> Namespace<'a> {
    /// The empty namespace.
    pub const GLOBAL: Self = Self(&[]);

    pub const fn new(names: &'a [Name]) -> Self {
        Self(names)
    }

    pub fn names(self) -> &'a [Name] {
        self.0
    }
}

impl Default for Namespace<'_> {
    fn default() -> Self {
        Self::GLOBAL
    }
}

impl Display for Namespace<'_> {
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
pub struct TypeName<'a> {
    pub namespace: Namespace<'a>,
    //parent_types: &'a [Name],
    pub name: Name,
}

impl<'a> TypeName<'a> {
    pub fn new(namespace: Namespace<'a>, name: Name) -> Self {
        Self { namespace, name }
    }
}
