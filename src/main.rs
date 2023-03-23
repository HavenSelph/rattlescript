mod lexer;
mod parser;
mod ast;
mod utils;

fn main() {
    let mut lex = lexer::Lexer::from_file(String::from("test.rat"));
    let tokens = lex.lex();
    let mut parser = parser::Parser::new(tokens);
    let ast = parser.parse();
    // let result = ast.run();
    println!("{:?}", ast);
}

