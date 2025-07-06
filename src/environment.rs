use std::collections::HashMap;

use crate::parser::Literal;

pub(super) struct Environment {
    scopes: Vec<HashMap<String, Literal>>,
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

    pub(super) fn define(&mut self, name: String, value: Literal) -> Result<(), EnvironmentError> {
        match self.scopes.last_mut() {
            Some(scope) => {
                scope.insert(name, value);
            }
            None => return Err(EnvironmentError::UndefinedScope),
        };

        Ok(())
    }

    pub(super) fn assign(&mut self, name: &str, value: Literal) -> Result<(), EnvironmentError> {
        for scope in self.scopes.iter_mut().rev() {
            if scope.contains_key(name) {
                scope.insert(name.to_string(), value);
                return Ok(());
            };
        }

        Err(EnvironmentError::UndefinedVariable(name.to_string()))
    }

    pub(super) fn get(&self, name: &str) -> Result<Literal, EnvironmentError> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                // TODO: probably shouldn't be cloning
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
