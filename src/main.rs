use std::io::Read;

mod lexer;
mod parser;
mod ast;
mod utils;
mod token;
mod interpreter;
mod builtins;

fn main() {
    // get filename from args or printout usage, optionally take file content from stdin if arg is "-"
    if std::env::args().len() != 2 {
        println!("Usage: rattlesnake <filename>");
        return;
    }

    let filename = std::env::args().nth(1).unwrap();
    let mut lex = if filename.as_str() == "-" {
        let mut input = String::new();
        std::io::stdin().read_to_string(&mut input).expect("Failed to read from stdin");
        lexer::Lexer::new(input, "<stdin>".to_string())
    } else {
        lexer::Lexer::from_file(filename)
    };

    let tokens = lex.lex();
    let mut parser = parser::Parser::new(tokens);
    let ast = parser.parse();
    let mut interpreter = interpreter::Interpreter::new();
    interpreter.execute(&ast);
}

