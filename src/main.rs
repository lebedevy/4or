use std::{
    env,
    io::{self, Write},
    process::exit,
};

use interpreter::{Interpreter, InterpreterError};
use parser::{Parser, ParserError};
use scanner::Scanner;

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
    loop {
        print!("> ");
        io::stdout().flush().expect("Failed to write to console");
        let mut input = String::new();
        let _ = io::stdin().read_line(&mut input);
        if input.is_empty() {
            break;
        }
        let _ = run(input);
    }
}

fn run_file(path: &String) {
    let content = std::fs::read_to_string(path);

    let content = match content {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Unable to read file {err}");
            exit(1)
        }
    };

    match run(content) {
        Ok(_) => {
            return;
        }
        Err(err) => {
            dbg!(err);
            exit(70);
        }
    }
}

fn run(content: String) -> Result<(), ProgramError> {
    let mut scanner = Scanner::new(content);

    let tokens = scanner.scan_tokens();
    let statements = Parser::parse(tokens.into_iter().enumerate().peekable());

    let statements = match statements {
        Ok(exp) => exp,
        Err(err) => {
            dbg!(&err);
            return Err(ProgramError::ParseError(err));
        }
    };

    Interpreter::run(statements)?;

    Ok(())
}

#[derive(Debug)]
enum ProgramError {
    InterpreterError(InterpreterError),
    ParseError(ParserError),
}

impl From<InterpreterError> for ProgramError {
    fn from(value: InterpreterError) -> Self {
        Self::InterpreterError(value)
    }
}
