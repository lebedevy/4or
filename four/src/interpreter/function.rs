use std::sync::{Arc, RwLock};

use crate::{
    InterpreterError,
    environment::Environment,
    interpreter::{Interpreter, Return, types::Types},
    parser::{
        Literal,
        statement::{Block, Identifier},
    },
};

pub(crate) mod native;

pub(crate) trait Fun {
    fn call(&self, interpreter: &mut Interpreter) -> Result<Types, InterpreterError>;
    fn get_name(&self) -> &Identifier;
    fn get_params(&self) -> &Vec<Identifier>;
    fn get_closure(&self) -> Arc<RwLock<Environment>>;
}

#[derive(Debug)]
pub(super) struct UserFn {
    pub(super) ident: Identifier,
    pub(super) params: Vec<Identifier>,
    pub(super) body: Block,
    pub(super) closure: Arc<RwLock<Environment>>,
}

impl UserFn {
    pub(super) fn new(
        ident: Identifier,
        params: Vec<Identifier>,
        body: Block,
        closure: Arc<RwLock<Environment>>,
    ) -> Self {
        Self {
            ident,
            params,
            body,
            closure,
        }
    }
}

impl Fun for UserFn {
    fn call(&self, interpreter: &mut Interpreter) -> Result<Types, InterpreterError> {
        for statement in &self.body.statements {
            if let Return::Return(val) = interpreter.execute(&statement)? {
                return Ok(val);
            }
        }

        Ok(Types::Primitive(Literal::Nil))
    }

    fn get_name(&self) -> &Identifier {
        &self.ident
    }

    fn get_params(&self) -> &Vec<Identifier> {
        &self.params
    }

    fn get_closure(&self) -> Arc<RwLock<Environment>> {
        self.closure.clone()
    }
}
