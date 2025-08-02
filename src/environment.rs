use std::{collections::HashMap, sync::Arc};

use crate::interpreter::{
    function::{Fun, native::PrintFn},
    types::{ReferenceTypes, Types},
};

pub(super) struct Environment {
    scope: HashMap<String, Types>,
    closure: Option<Box<Environment>>,
}

impl Environment {
    pub(super) fn new() -> Self {
        let print_fn = PrintFn::new();

        Self {
            scope: vec![(
                print_fn.get_name().to_string(),
                Types::Reference(ReferenceTypes::Function(Arc::new(print_fn))),
            )]
            .into_iter()
            .collect(),

            closure: None,
        }
    }

    pub(super) fn create_scope(closure: Environment) -> Self {
        Self {
            scope: HashMap::new(),
            closure: Some(Box::new(closure)),
        }
    }

    pub(super) fn close(self) -> Option<Box<Environment>> {
        self.closure
    }

    pub(super) fn define(&mut self, name: &str, value: Types) -> Result<(), EnvironmentError> {
        self.scope.insert(name.to_string(), value);

        Ok(())
    }

    pub(super) fn assign(&mut self, name: &str, value: Types) -> Result<(), EnvironmentError> {
        if self.scope.contains_key(name) {
            self.scope.insert(name.to_string(), value);
            return Ok(());
        };

        match &mut self.closure {
            Some(closure) => closure.assign(name, value),
            None => Err(EnvironmentError::UndefinedVariable(name.to_string())),
        }
    }

    pub(super) fn get(&self, name: &str) -> Result<Types, EnvironmentError> {
        match self.scope.get(name) {
            Some(val) => Ok(val.clone()),
            None => match &self.closure {
                Some(closure) => closure.get(name),
                None => Err(EnvironmentError::UndefinedVariable(name.to_string())),
            },
        }
    }
}

pub(crate) enum EnvironmentError {
    UndefinedVariable(String),
}
