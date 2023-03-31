#![allow(clippy::upper_case_acronyms)]

use crate::error::Result;

mod ast;
mod error;
mod interpreter;
mod lexer;
mod parser;
mod common;
mod repl;
mod token;


fn run_file(filename: &str) -> Result<()> {
    let content = std::fs::read_to_string(filename).expect("Couldn't open input file");

    let mut lex = lexer::Lexer::new(content, Box::leak(filename.to_string().into_boxed_str()));
    let tokens = lex.lex()?;

    let mut parser = parser::Parser::new(tokens);
    let ast = parser.parse()?;

    let mut interpreter = interpreter::Interpreter::new();
    interpreter.execute(&ast)?;
    Ok(())
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();
    if args.len() == 1 {
        let mut repl = repl::Repl::new();
        repl.run();
        std::process::exit(0);
    }

    match run_file(&args[1]) {
        Ok(_) => std::process::exit(0),
        Err(err) => {
            err.print_with_source();
            std::process::exit(1);
        }
    }
}
