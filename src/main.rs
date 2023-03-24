mod lexer;
mod parser;
mod ast;
mod utils;
mod token;
mod interpreter;
mod builtins;

fn main() {
    let mut lex = lexer::Lexer::from_file(String::from("test.rat"));
    let tokens = lex.lex();
    let mut parser = parser::Parser::new(tokens);
    let ast = parser.parse();
    let mut interpreter = interpreter::Interpreter::new();
    let result = interpreter.run(&ast);
    println!("{:?}", result);
}

