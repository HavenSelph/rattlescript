#![allow(clippy::upper_case_acronyms)]

use crate::error::Result;
use std::process::exit;

mod ast;
mod builtin;
mod error;
mod interpreter;
mod lexer;
mod parser;
mod repl;
mod token;
mod value;

fn main() -> Result<()> {
    let args = Args::get_args();
    if args.repl && args.file.is_some() {
        println!("Cannot run file and repl at the same time.");
        exit(1);
    } else if args.file.is_some() && args.code.is_some() {
        println!("Cannot run file and pass --code or -c at the same time.");
        exit(1);
    }
    if args.repl {
        let mut repl = repl::Repl::new();
        repl.run();
        exit(0)
    }
    let file = if let Some(ref file) = args.file {
        std::fs::read_to_string(file).expect("Couldn't open input file")
    } else if let Some(code) = args.code {
        code
    } else {
        unreachable!()
    };
    let mut lex = lexer::Lexer::new(file, args.file.unwrap_or(String::from("<input>")));
    let tokens = lex.lex()?;
    let mut parser = parser::Parser::new(tokens);
    let ast = parser.parse()?;
    let mut interpreter = interpreter::Interpreter::new();
    interpreter.execute(&ast)?;
    Ok(())
}

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
            return Args {
                repl: true,
                file: None,
                code: None,
            };
        }

        let mut repl = None;
        let mut file = None;
        let mut code = None;
        let mut i: usize = 1;
        while i < args.len() {
            let item = &args[i];
            match item.as_str() {
                "--repl" | "-r" => {
                    repl = if repl.is_some() {
                        println!("Bad usage of repl param.");
                        println!("Usage: rattlesnake [file] [args]");
                        exit(1);
                    } else {
                        Some(true)
                    }
                }
                "--file" | "-f" => {
                    file = if file.is_some() {
                        println!("Multiple usages of file param.");
                        println!("Usage: rattlesnake [file] [args]");
                        exit(1);
                    } else {
                        i += 1;
                        Some(args[i].clone())
                    };
                }
                "--code" | "-c" => {
                    code = if code.is_some() {
                        println!("Multiple usages of code param.");
                        println!("Usage: rattlesnake [file] [args]");
                        exit(1);
                    } else {
                        i += 1;
                        Some(args[i].clone())
                    };
                }
                _ if !(item.starts_with('-') || item.starts_with("--")) && i == 1 => {
                    file = Some(item.clone());
                }
                _ => {
                    println!("Unknown argument \"{}\".", item);
                    println!("Usage: rattlesnake [file] [args]");
                    exit(1);
                }
            }
            i += 1;
        }
        Args {
            repl: repl.unwrap_or(false),
            file,
            code,
        }
    }
}
