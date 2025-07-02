use std::{
    env,
    io::{self, Write},
    process::exit,
};

use parser::Parser;
use scanner::Scanner;

mod parser;
mod scanner;
mod token;

fn main() {
    // Skip the first argument which is tradtionally the path to the executable
    let args: Vec<String> = env::args().skip(1).collect();

    dbg!(&args, &args.len());

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
    println!("> {content}");

    let mut scanner = Scanner::new(content);

    let tokens = scanner.scan_tokens();
    let expression = Parser::parse(tokens.into_iter().enumerate().peekable());

    dbg!(expression);
}
