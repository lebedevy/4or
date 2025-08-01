use crate::{
    InterpreterError,
    interpreter::{Interpreter, Return, types::Types},
    parser::{Literal, Statement},
    token::Token,
};

pub(crate) mod native;

pub(crate) trait Fun {
    fn call(&self, interpreter: &mut Interpreter) -> Result<Types, InterpreterError>;
    fn get_name(&self) -> &str;
    fn get_params(&self) -> &Vec<Token>;
}

#[derive(Debug)]
pub(super) struct UserFn {
    pub(super) name: String,
    pub(super) params: Vec<Token>,
    pub(super) body: Box<Statement>,
}

impl UserFn {
    pub(super) fn new(name: &str, params: Vec<Token>, body: Box<Statement>) -> Self {
        Self {
            name: name.to_string(),
            params,
            body,
        }
    }
}

impl Fun for UserFn {
    fn call(&self, interpreter: &mut Interpreter) -> Result<Types, InterpreterError> {
        let statements = match self.body.as_ref() {
            Statement::Block(statements) => statements,
            _ => {
                return Err(InterpreterError::InvalidFunctionBody(self.name.to_string()));
            }
        };

        for statement in statements {
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
}
