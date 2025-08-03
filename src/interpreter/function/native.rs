use std::sync::{Arc, RwLock};

use crate::{
    InterpreterError,
    environment::Environment,
    interpreter::{function::Fun, types::Types},
    token::{Token, TokenType},
};

pub(crate) struct PrintFn {
    params: Vec<Token>,
    closure: Arc<RwLock<Environment>>,
}

impl PrintFn {
    pub(crate) fn new(closure: Arc<RwLock<Environment>>) -> Self {
        Self {
            params: vec![Token {
                token_type: TokenType::Identifier("value".to_owned()),
                index: 0,
            }],
            closure,
        }
    }
}

impl Fun for PrintFn {
    fn call(
        &self,
        interpreter: &mut crate::interpreter::Interpreter,
    ) -> Result<Types, crate::InterpreterError> {
        let param = match &self
            .get_params()
            .first()
            .expect("Native print function not configured properly")
            .token_type
        {
            TokenType::Identifier(iden) => iden,
            _ => {
                return Err(InterpreterError::InvalidFunctionCall(
                    "Missing print 'value' parameter".to_owned(),
                ));
            }
        };

        println!("{}", interpreter.get_variable(&param)?);

        Ok(Types::Primitive(crate::parser::Literal::Nil))
    }

    fn get_name(&self) -> &str {
        "print"
    }

    fn get_params(&self) -> &Vec<Token> {
        &self.params
    }

    fn get_closure(&self) -> std::sync::Arc<std::sync::RwLock<crate::environment::Environment>> {
        self.closure.clone()
    }
}
