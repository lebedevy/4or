use std::sync::{Arc, RwLock};

use crate::{
    environment::Environment,
    interpreter::{function::Fun, types::Types},
    parser::{
        Statement,
        statement::{Block, Identifier},
    },
    token::{Token, TokenType},
};

pub(crate) struct PrintFn {
    ident: Identifier,
    params: Vec<Identifier>,
    closure: Arc<RwLock<Environment>>,
}

impl PrintFn {
    pub(crate) fn new(closure: Arc<RwLock<Environment>>) -> Self {
        Self {
            ident: Identifier::new(
                "print",
                Token {
                    token_type: TokenType::Identifier("print".to_owned()),
                    index: 0,
                },
            ),
            params: vec![Identifier::new(
                "value",
                Token {
                    token_type: TokenType::Identifier("value".to_owned()),
                    index: 0,
                },
            )],
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

        println!("{}", interpreter.get_variable(&param)?);

        Ok(Types::Primitive(crate::parser::Literal::Nil))
    }

    fn get_name(&self) -> &Identifier {
        &self.ident
    }

    fn get_params(&self) -> &Vec<Identifier> {
        &self.params
    }

    fn get_closure(&self) -> std::sync::Arc<std::sync::RwLock<crate::environment::Environment>> {
        self.closure.clone()
    }
}

pub(crate) fn get_statement_from_fn(fun: &impl Fun) -> Statement {
    Statement::Function(
        fun.get_name().clone(),
        fun.get_params().clone(),
        Block { statements: vec![] },
    )
}
