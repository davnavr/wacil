//! Representation of a C# source code file.

use std::fmt::{Display, Formatter, Result, Write};

pub use wacil_bindgen::interface::types::{ReturnType, Type};
pub use wacil_bindgen::interface::{Name, Namespace};

#[repr(transparent)]
struct CommaSeparated<'a, T>(&'a [T]);

impl<'a, T> Display for CommaSeparated<'a, T>
where
    T: Display,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.0.iter().enumerate().try_for_each(|(index, item)| {
            if index > 0 {
                f.write_char(',')?;
            }
            item.fmt(f)
        })
    }
}

pub enum Expression<'a> {
    NewInstance(&'a Type, &'a [Expression<'a>]),
}

impl Display for Expression<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::NewInstance(ty, args) => {
                write!(f, "new {ty}({})", &CommaSeparated(args))
            }
        }
    }
}

#[derive(Clone, Copy)]
pub enum AccessModifier {
    Public,
    Private,
}

impl Display for AccessModifier {
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.write_str(match self {
            Self::Public => "public",
            Self::Private => "private",
        })
    }
}

pub enum FieldModifier {
    Readonly,
}

impl Display for FieldModifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        f.write_str(match self {
            Self::Readonly => "readonly",
        })
    }
}

pub struct Field<'a> {
    pub access: AccessModifier,
    pub modifiers: &'a [FieldModifier],
    pub name: Name,
    pub value_type: &'a Type,
    pub value: Option<&'a Expression<'a>>,
}

impl Display for Field<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} ", self.access)?;
        self.modifiers.iter().try_for_each(|m| write!(f, "{m} "))?;
        write!(f, "{} {}", self.value_type, self.name)?;
        if let Some(expression) = self.value {
            write!(f, " = {expression}")?;
        }
        f.write_char(';')
    }
}

pub enum Member<'a> {
    Field(Field<'a>),
}

impl Display for Member<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Field(field) => field.fmt(f),
        }
    }
}

pub enum TypeDefinitionKind {
    Class,
}

pub struct TypeDefinition<'a> {
    pub access: AccessModifier,
    pub kind: TypeDefinitionKind,
    pub name: Name,
    pub sub_types: &'a [Type],
    pub members: &'a [Member<'a>],
}

impl Display for TypeDefinition<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} ", self.access)?;
        f.write_str(match self.kind {
            TypeDefinitionKind::Class => "class",
        })?;
        write!(f, " {}", self.name)?;
        if !self.sub_types.is_empty() {
            f.write_str(" : ")?;
            CommaSeparated(&self.sub_types).fmt(f)?;
        }
        f.write_char('{')?;
        self.members.iter().try_for_each(|m| write!(f, "{m}"))?;
        f.write_char('}')
    }
}

pub struct SourceCode<'a> {
    pub namespace: Namespace,
    pub type_definitions: &'a [TypeDefinition<'a>],
}

impl Display for SourceCode<'_> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        let has_namespace = !self.namespace.names().is_empty();
        if has_namespace {
            write!(f, "namespace {} {{", self.namespace)?;
        }
        self.type_definitions.iter().try_for_each(|ty| write!(f, "{ty}"))?;
        if has_namespace {
            write!(f, "}}")?;
        }
        Ok(())
    }
}
