use std::collections::HashMap;

use crate::parser::Literal;

pub(super) struct Environment {
    values: HashMap<String, Literal>,
}

impl Environment {
    pub(super) fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub(super) fn define(&mut self, name: String, value: Literal) {
        self.values.insert(name, value);
    }

    pub(super) fn get(&self, name: &str) -> Result<Literal, EnvironmentError> {
        match self.values.get(name) {
            // TODO: probably shouldn't be cloning?
            Some(value) => Ok(value.clone()),
            None => Err(EnvironmentError::UndefinedVariable(name.to_string())),
        }
    }
}

pub(crate) enum EnvironmentError {
    UndefinedVariable(String),
}
