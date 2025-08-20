use std::{
    env,
    io::{self, Write},
    process::exit,
};

use four::{interpreter::Interpreter, resolver::Resolver, run};

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
    let mut resolver = Resolver::new();
    let mut interpreter = Interpreter::new(&mut resolver);
    loop {
        print!("> ");
        io::stdout().flush().expect("Failed to write to console");
        let mut input = String::new();
        let _ = io::stdin().read_line(&mut input);
        if input.is_empty() {
            break;
        }

        if let Err(err) = run(&mut interpreter, &mut resolver, input.clone()) {
            eprintln!("{}", err.get_error_text(&input));
        }
    }
}

fn run_file(path: &String) {
    let content = std::fs::read_to_string(path);
    let mut resolver = Resolver::new();
    let mut interpreter = Interpreter::new(&mut resolver);

    let content = match content {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Unable to read file {err}");
            exit(1)
        }
    };

    match run(&mut interpreter, &mut resolver, content) {
        Ok(_) => {
            return;
        }
        Err(err) => {
            dbg!(err);
            exit(70);
        }
    }
}
