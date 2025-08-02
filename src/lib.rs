use std::fmt::Display;

use itertools::min;

use crate::{
    interpreter::{Interpreter, InterpreterError},
    parser::{Parser, ParserError},
    scanner::Scanner,
    token::Token,
};

pub mod environment;
pub mod interpreter;
pub mod parser;
pub mod scanner;
pub mod token;

pub fn run(interpreter: &mut Interpreter, content: String) -> Result<(), ProgramError> {
    let mut scanner = Scanner::new(content);

    let tokens = scanner.scan_tokens();
    let statements = Parser::parse(tokens.into_iter().peekable())?;

    interpreter.run(statements)?;

    Ok(())
}

#[derive(Debug)]
pub enum ProgramError {
    InterpreterError(InterpreterError),
    ParseError(ParserError),
}

fn get_token_error_text(input: &str, token: &Token) -> String {
    let mut text = "".to_owned();
    let chars = input.chars();
    let start = if token.index < 10 {
        0
    } else {
        token.index - 10
    };
    let end = min(vec![input.len(), token.index + 10]).expect("Could not get min for end");
    let start_spacing = " ".repeat(token.index - start);

    // return 10 chars before and after
    text += format!("{}\n", chars.skip(start).take(end).collect::<String>()).as_str();
    text += format!("{}^\n", start_spacing).as_str();

    text
}

impl ProgramError {
    pub fn get_error_text(&self, input: &str) -> String {
        let mut error = "".to_owned();
        if let Some(token) = match self {
            ProgramError::InterpreterError(interpreter_error) => match interpreter_error {
                InterpreterError::InvalidUnary(token)
                | InterpreterError::InvalidVariableIdentifier(token)
                | InterpreterError::InvalidBinary(token)
                | InterpreterError::InvalidLogicalOperator(token) => Some(token.clone()),
                _ => None,
            },
            // TODO
            ProgramError::ParseError(parser_error) => match parser_error {
                ParserError::UnexpectedToken(token, _) => token.clone(),
                ParserError::InvalidPrimaryToken(token) => Some(token.clone()),
                ParserError::ExpectedIdentifier(token) => token.clone(),
                _ => None,
            },
        } {
            error = get_token_error_text(input, &token);
        }
        error += format!("{}", self).as_str();

        error
    }
}

impl Display for ProgramError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ProgramError::InterpreterError(interpreter_error) => {
                write!(f, "Program error - {}", interpreter_error)?
            }
            ProgramError::ParseError(parser_error) => write!(f, "{}", parser_error)?,
        };

        Ok(())
    }
}

impl From<InterpreterError> for ProgramError {
    fn from(value: InterpreterError) -> Self {
        Self::InterpreterError(value)
    }
}

impl From<ParserError> for ProgramError {
    fn from(value: ParserError) -> Self {
        Self::ParseError(value)
    }
}
