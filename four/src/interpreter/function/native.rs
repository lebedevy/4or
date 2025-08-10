use std::sync::{Arc, RwLock};

use crate::{
    environment::Environment,
    interpreter::{function::Fun, types::Types},
    parser::statement::Identifier,
    token::{Token, TokenType},
};

pub(crate) struct PrintFn {
    params: Vec<Identifier>,
    closure: Arc<RwLock<Environment>>,
}

impl PrintFn {
    pub(crate) fn new(closure: Arc<RwLock<Environment>>) -> Self {
        Self {
            params: vec![Identifier {
                ident: "value".to_owned(),
                token: Token {
                    token_type: TokenType::Identifier("value".to_owned()),
                    index: 0,
                },
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
        let param = &self
            .get_params()
            .first()
            .expect("Native print function not configured properly");

        println!("{}", interpreter.get_variable(&param.ident)?);

        Ok(Types::Primitive(crate::parser::Literal::Nil))
    }

    fn get_name(&self) -> &str {
        "print"
    }

    fn get_params(&self) -> &Vec<Identifier> {
        &self.params
    }

    fn get_closure(&self) -> std::sync::Arc<std::sync::RwLock<crate::environment::Environment>> {
        self.closure.clone()
    }
}
