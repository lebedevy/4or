use std::{collections::HashMap, fmt::Display, sync::Arc};

use crate::{
    parser::{Literal, Statement},
    token::Token,
};

#[derive(Debug, Clone)]
pub(super) enum Types {
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

#[derive(Debug)]
pub(super) struct FnRef {
    pub(super) name: String,
    pub(super) params: Vec<Token>,
    pub(super) body: Box<Statement>,
}

impl FnRef {
    pub(super) fn new(name: &str, params: Vec<Token>, body: Box<Statement>) -> Self {
        Self {
            name: name.to_string(),
            params,
            body,
        }
    }
}

#[derive(Debug, Clone)]
pub(super) enum ReferenceTypes {
    Function(Arc<FnRef>),
}

impl Display for ReferenceTypes {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ReferenceTypes::Function(func) => write!(f, "Fn '{}' reference", func.name),
        }
    }
}

pub(super) struct Environment {
    scopes: Vec<HashMap<String, Types>>,
}

impl Environment {
    pub(super) fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    pub(super) fn create_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    // It is currently possible to remove all scopes
    pub(super) fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    pub(super) fn define(&mut self, name: &str, value: Types) -> Result<(), EnvironmentError> {
        match self.scopes.last_mut() {
            Some(scope) => {
                scope.insert(name.to_string(), value);
            }
            None => return Err(EnvironmentError::UndefinedScope),
        };

        Ok(())
    }

    pub(super) fn assign(&mut self, name: &str, value: Types) -> Result<(), EnvironmentError> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value);
                return Ok(());
            };
        }

        Err(EnvironmentError::UndefinedVariable(name.to_string()))
    }

    pub(super) fn get(&self, name: &str) -> Result<Types, EnvironmentError> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                // TODO: Think more about cloning
                return Ok(value.clone());
            }
        }

        Err(EnvironmentError::UndefinedVariable(name.to_string()))
    }
}

pub(crate) enum EnvironmentError {
    UndefinedVariable(String),
    UndefinedScope,
}
