use std::{
    collections::HashMap,
    sync::{Arc, PoisonError, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

use crate::interpreter::types::Types;

#[derive(Debug)]
pub(super) struct Environment {
    scope: HashMap<String, Types>,
    closure: Option<Arc<RwLock<Environment>>>,
}

impl Environment {
    pub(super) fn new() -> Self {
        Self {
            scope: HashMap::new(),
            closure: None,
        }
    }

    pub(super) fn create_with_closure(closure: Arc<RwLock<Environment>>) -> Self {
        Self {
            scope: HashMap::new(),
            closure: Some(closure),
        }
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
            Some(closure) => Ok(closure.write()?.assign(name, value)?),
            None => Err(EnvironmentError::UndefinedVariable(name.to_string())),
        }
    }

    pub(super) fn get(&self, name: &str) -> Result<Types, EnvironmentError> {
        if let Some(val) = self.scope.get(name) {
            return Ok(val.clone());
        }

        match &self.closure {
            Some(closure) => closure.read()?.get(name),
            None => Err(EnvironmentError::UndefinedVariable(name.to_string())),
        }
    }
}

#[derive(Debug)]
pub(crate) enum EnvironmentError {
    UndefinedVariable(String),
    ReferencePoison(String),
}

impl From<PoisonError<RwLockReadGuard<'_, Environment>>> for EnvironmentError {
    fn from(value: PoisonError<RwLockReadGuard<'_, Environment>>) -> Self {
        EnvironmentError::ReferencePoison(value.to_string())
    }
}

impl From<PoisonError<RwLockWriteGuard<'_, Environment>>> for EnvironmentError {
    fn from(value: PoisonError<RwLockWriteGuard<'_, Environment>>) -> Self {
        EnvironmentError::ReferencePoison(value.to_string())
    }
}
