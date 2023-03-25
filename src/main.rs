use std::io::{Read, Write};
use std::process::exit;
use crate::lexer::Lexer;

mod lexer;
mod parser;
mod ast;
mod utils;
mod token;
mod interpreter;
mod builtins;
mod value;


#[derive(Debug)]
struct Args {
    repl: bool,
    file: Option<String>,
    code: Option<String>,
}


impl Args {
    fn get_args() -> Args {
        let args: Vec<String> = std::env::args().collect();
        if args.len() == 1 {
            Args {
                repl: true,
                file: None,
                code: None,
            }
        } else {
            let mut repl = None;
            let mut file = None;
            let mut code = None;
            let mut i: usize = 1;
            while i < args.len() {
                let item = &args[i];
                if item == "--repl" || item == "-r" {
                    repl = match repl {
                        Some(_) => {
                            println!("Bad usage of repl param.");
                            println!("Usage: rattlesnake [file] [args]");
                            exit(1);
                        },
                        None => Some(true),
                    };
                } else if item == "--file" || item == "-f" {
                    file = match file {
                        Some(_) => {
                            println!("Multiple usages of file param.");
                            println!("Usage: rattlesnake [file] [args]");
                            exit(1);
                        },
                        None => {
                            i += 1;
                            Some(args[i].clone())
                        },
                    };
                } else if item == "--code" || item == "-c" {
                    code = match code {
                        Some(_) => {
                            println!("Multiple usages of code param.");
                            println!("Usage: rattlesnake [file] [args]");
                            exit(1);
                        },
                        None => {
                            i += 1;
                            Some(args[i].clone())
                        },
                    };
                } else if !(item.starts_with('-') || item.starts_with("--")) && i == 1 {
                    file = Some(item.clone());
                } else {
                    println!("Unknown argument \"{}\".", item);
                    println!("Usage: rattlesnake [file] [args]");
                    exit(1);
                }
                i += 1;
            }
            Args {
                repl: repl.unwrap_or(false),
                file,
                code
            }
        }
    }
}


fn need_more_input(input: &String) -> bool {
    false
}


fn main() {
    let args = Args::get_args();
    if args.repl && !args.file.is_some() {
        unimplemented!("Repl is not implemented yet.")
    } else if args.file.is_some() && !args.repl && !args.code.is_some() {
        let mut lex = lexer::Lexer::from_file(args.file.unwrap());
        let tokens = lex.lex();
        let mut parser = parser::Parser::new(tokens);
        let ast = parser.parse();
        let mut interpreter = interpreter::Interpreter::new();
        interpreter.execute(&ast);
    } else if args.code.is_some() && !args.file.is_some() {
        let mut lex = lexer::Lexer::new(args.code.unwrap(), String::from("<string>"));
        let tokens = lex.lex();
        let mut parser = parser::Parser::new(tokens);
        let ast = parser.parse();
        let mut interpreter = interpreter::Interpreter::new();
        interpreter.execute(&ast);
    } else {
        println!("Bad arguments passed");
        println!("Usage: rattlesnake [file] [args]");
        exit(1);
    }
}
