use std::{
    env,
    fmt::Display,
    io::{self, Write},
    process::exit,
};

use interpreter::{Interpreter, InterpreterError};
use itertools::min;
use parser::{Parser, ParserError};
use scanner::Scanner;
use token::Token;

mod environment;
mod interpreter;
mod parser;
mod scanner;
mod token;

fn main() {
    // Skip the first argument which is tradtionally the path to the executable
    let args: Vec<String> = env::args().skip(1).collect();

    match &args.len() {
        0 => run_prompt(),
        1 => run_file(&args[0]),
        _ => {
            println!("Usage: four [script]");
            exit(64);
        }
    }
}

fn run_prompt() {
    let mut interpreter = Interpreter::new();
    loop {
        print!("> ");
        io::stdout().flush().expect("Failed to write to console");
        let mut input = String::new();
        let _ = io::stdin().read_line(&mut input);
        if input.is_empty() {
            break;
        }

        if let Err(err) = run(&mut interpreter, input.clone()) {
            match &err {
                ProgramError::InterpreterError(interpreter_error) => match interpreter_error {
                    InterpreterError::InvalidUnary(token) => print_token_error(input, token),
                    InterpreterError::InvalidBinary(token) => print_token_error(input, token),
                    InterpreterError::InvalidLogicalOperator(token) => {
                        print_token_error(input, token)
                    }
                    InterpreterError::InvalidVariableIdentifier(token) => {
                        print_token_error(input, token)
                    }
                    _ => (),
                },
                // TODO
                ProgramError::ParseError(parser_error) => match parser_error {
                    ParserError::UnexpectedToken(token, _token_type) => {
                        if token.is_some() {
                            print_token_error(input, &token.as_ref().unwrap());
                        }
                    }
                    ParserError::InvalidPrimaryToken(token) => todo!(),
                    ParserError::UnexpectedTermination => todo!(),
                    ParserError::ExpectedIdentifier(token) => todo!(),
                    ParserError::TooManyParameters(_) => todo!(),
                },
            }
            eprintln!("{}", err);
        }
    }
}

fn print_token_error(input: String, token: &Token) {
    let chars = input.chars();
    let start = min(vec![0, token.index - 10]).expect("Could not get min");
    let end = min(vec![input.len(), token.index + 10]).expect("Could not get min for end");
    let start_spacing = " ".repeat(token.index - start);

    // return 10 chars before and after
    println!("{}", chars.skip(start).take(end).collect::<String>());
    println!("{}^", start_spacing);
}

fn run_file(path: &String) {
    let content = std::fs::read_to_string(path);
    let mut interpreter = Interpreter::new();

    let content = match content {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Unable to read file {err}");
            exit(1)
        }
    };

    match run(&mut interpreter, content) {
        Ok(_) => {
            return;
        }
        Err(err) => {
            dbg!(err);
            exit(70);
        }
    }
}

fn run(interpreter: &mut Interpreter, content: String) -> Result<(), ProgramError> {
    let mut scanner = Scanner::new(content);

    let tokens = scanner.scan_tokens();
    let statements = Parser::parse(tokens.into_iter().enumerate().peekable())?;

    interpreter.run(statements)?;

    Ok(())
}

#[derive(Debug)]
enum ProgramError {
    InterpreterError(InterpreterError),
    ParseError(ParserError),
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
