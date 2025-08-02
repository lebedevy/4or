use crate::{
    InterpreterError,
    interpreter::{function::Fun, types::Types},
    token::{Token, TokenType},
};

pub(crate) struct PrintFn {
    params: Vec<Token>,
}

impl PrintFn {
    pub(crate) fn new() -> Self {
        Self {
            params: vec![Token {
                token_type: TokenType::Identifier("value".to_owned()),
                index: 0,
            }],
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
}
