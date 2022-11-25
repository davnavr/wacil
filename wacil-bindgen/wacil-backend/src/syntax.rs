//! Representation of a C# source code file.

use std::fmt::{Display, Formatter, Result, Write};

pub use wacil_bindgen::interface::types::{ReturnType, Type};
pub use wacil_bindgen::interface::{Name, Namespace, TypeName};

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
    NewInstance(&'a Type<'a>, &'a [Expression<'a>]),
    This,
    Identifier(Name),
    MemberAccess(&'a Expression<'a>, Name),
}

impl Display for Expression<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::NewInstance(ty, args) => {
                write!(f, "new {ty}({})", &CommaSeparated(args))
            }
            Self::This => f.write_str("this"),
            Self::Identifier(name) => f.write_str(name),
            Self::MemberAccess(e, name) => write!(f, "{e}.{name}"),
        }
    }
}

pub enum Statement<'a> {
    Expression(Expression<'a>),
    Assignment {
        destination: Expression<'a>,
        value: Expression<'a>,
    },
}

impl Display for Statement<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Expression(expr) => write!(f, "{expr};"),
            Self::Assignment { destination, value } => write!(f, "{destination} = {value};"),
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
    pub value_type: &'a Type<'a>,
    pub value: Option<&'a Expression<'a>>,
}

pub struct Parameter<'a> {
    pub argument_type: &'a Type<'a>,
    pub name: Name,
}

impl Display for Parameter<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} {}", self.argument_type, self.name)
    }
}

pub enum ConstructorBaseCall<'a> {
    None,
    Base(&'a [Expression<'a>]),
    This(&'a [Expression<'a>]),
}

impl Display for ConstructorBaseCall<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::None => Ok(()),
            Self::Base(arguments) => write!(f, "base({})", CommaSeparated(arguments)),
            Self::This(arguments) => write!(f, "this({})", CommaSeparated(arguments)),
        }
    }
}

pub struct Constructor<'a> {
    pub access: AccessModifier,
    pub parameters: &'a [Parameter<'a>],
    pub base_call: ConstructorBaseCall<'a>,
    pub body: &'a [Statement<'a>],
}

pub enum Member<'a> {
    Field(Field<'a>),
    Constructor(Constructor<'a>),
}

pub enum TypeDefinitionKind {
    Class,
}

pub struct TypeDefinition<'a> {
    pub access: AccessModifier,
    pub kind: TypeDefinitionKind,
    pub name: Name,
    pub sub_types: &'a [&'a Type<'a>],
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
        for member in self.members.iter() {
            match member {
                Member::Field(field) => {
                    write!(f, "{} ", field.access)?;
                    field.modifiers.iter().try_for_each(|m| write!(f, "{m} "))?;
                    write!(f, "{} {}", field.value_type, field.name)?;
                    if let Some(expression) = field.value {
                        write!(f, " = {expression}")?;
                    }
                    f.write_char(';')?;
                }
                Member::Constructor(ctor) => {
                    write!(f, "{} {}({})", ctor.access, self.name, CommaSeparated(ctor.parameters))?;
                    if !matches!(ctor.base_call, ConstructorBaseCall::None) {
                        write!(f, ":{}", ctor.base_call)?;
                    }
                    f.write_char('{')?;
                    ctor.body.iter().try_for_each(|s| s.fmt(f))?;
                    f.write_char('}')?;
                }
            }
        }
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
