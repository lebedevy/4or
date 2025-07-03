use std::{
    env,
    io::{self, Write},
    process::exit,
};

use interpreter::Interpreter;
use parser::Parser;
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
        run(input);
    }
}

fn run_file(path: &String) {
    let content = std::fs::read_to_string(path);

    match content {
        Ok(content) => run(content),
        Err(err) => {
            eprintln!("Unable to read file {err}");
            exit(1)
        }
    }
}

fn run(content: String) {
    let mut scanner = Scanner::new(content);

    let tokens = scanner.scan_tokens();
    let expression = Parser::parse(tokens.into_iter().enumerate().peekable());

    let expression = match expression {
        Ok(exp) => exp,
        Err(err) => {
            dbg!(err);
            return;
        }
    };

    let val = Interpreter::evaluate(expression);

    match val {
        Ok(val) => match val {
            parser::Literal::Bool(val) => {
                println!("> {}", val);
            }
            parser::Literal::Number(val) => {
                println!("> {}", val);
            }
            parser::Literal::String(val) => {
                println!("> {}", val);
            }
            parser::Literal::Nil => {
                println!("> {}", "nil");
            }
        },
        Err(err) => {
            dbg!(err);
        }
    };
}
