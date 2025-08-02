use std::{fmt::Debug, fmt::Display, sync::Arc};

use crate::{interpreter::function::Fun, parser::Literal};

#[derive(Clone, Debug)]
pub(crate) enum Types {
    Primitive(Literal),
    Reference(ReferenceTypes),
}

impl Display for Types {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Types::Primitive(literal) => write!(f, "{}", literal.to_string()),
            Types::Reference(reference_types) => write!(f, "{}", reference_types.to_string()),
        }
    }
}

#[derive(Clone)]
pub(crate) enum ReferenceTypes {
    Function(Arc<dyn Fun>),
}

impl Debug for ReferenceTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReferenceTypes::Function(fun) => write!(f, "Fn '{}' reference", fun.get_name()),
        }
    }
}

impl Display for ReferenceTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReferenceTypes::Function(func) => write!(f, "Fn '{}' reference", func.get_name()),
        }
    }
}
