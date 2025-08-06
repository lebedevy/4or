use std::sync::{Arc, RwLock};

use crate::{
    InterpreterError,
    environment::Environment,
    interpreter::{Interpreter, Return, types::Types},
    parser::{Literal, statement::Block},
    token::Token,
};

pub(crate) mod native;

pub(crate) trait Fun {
    fn call(&self, interpreter: &mut Interpreter) -> Result<Types, InterpreterError>;
    fn get_name(&self) -> &str;
    fn get_params(&self) -> &Vec<Token>;
    fn get_closure(&self) -> Arc<RwLock<Environment>>;
}

#[derive(Debug)]
pub(super) struct UserFn {
    pub(super) name: String,
    pub(super) params: Vec<Token>,
    pub(super) body: Block,
    pub(super) closure: Arc<RwLock<Environment>>,
}

impl UserFn {
    pub(super) fn new(
        name: &str,
        params: Vec<Token>,
        body: Block,
        closure: Arc<RwLock<Environment>>,
    ) -> Self {
        Self {
            name: name.to_string(),
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

    fn get_name(&self) -> &str {
        &self.name
    }

    fn get_params(&self) -> &Vec<Token> {
        &self.params
    }

    fn get_closure(&self) -> Arc<RwLock<Environment>> {
        self.closure.clone()
    }
}
