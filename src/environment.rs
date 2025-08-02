use std::{collections::HashMap, sync::Arc};

use crate::interpreter::{
    function::{Fun, native::PrintFn},
    types::{ReferenceTypes, Types},
};

pub(super) struct Environment {
    scopes: Vec<HashMap<String, Types>>,
}

impl Environment {
    pub(super) fn new() -> Self {
        let print_fn = PrintFn::new();

        Self {
            scopes: vec![
                vec![(
                    print_fn.get_name().to_string(),
                    Types::Reference(ReferenceTypes::Function(Arc::new(print_fn))),
                )]
                .into_iter()
                .collect(),
            ],
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
