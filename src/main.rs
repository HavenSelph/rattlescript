#![allow(clippy::upper_case_acronyms)]
use crate::error::Result;

mod ast;
mod common;
mod error;
mod interpreter;
mod lexer;
mod parser;
mod repl;
mod token;

fn run_file(filename: &str, verbose: bool) -> Result<()> {
    let content = std::fs::read_to_string(filename).expect("Couldn't open input file");

    let mut lex = lexer::Lexer::new(content, Box::leak(filename.to_string().into_boxed_str()));
    let tokens = lex.lex()?;

    if verbose {
        for token in &tokens {
            println!("{:?}", token);
        }
    }

    let mut parser = parser::Parser::new(tokens);
    let ast = parser.parse()?;

    if verbose {
        println!("{:#?}", ast);
    }

    let mut interpreter = interpreter::Interpreter::new();
    interpreter.execute(&ast)?;
    Ok(())
}

fn print_help(filename: &str) {
    println!("Usage: {} [options] [filename]", filename);
    println!("Options:");
    println!("  -d, --disable-error-context   Disable error context (default: false)");
    println!("  -v, --verbose                 Enable verbose output (default: false)");
    println!("  -i, --info                    Print info about the REPL");
    println!("  -l, --license                 Print the license");
    println!("  -h, --help                    Print this help message");
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();

    let mut filename = None;
    let mut disable_error_context = false;
    let mut verbose = false;

    for arg in args.iter().skip(1) {
        match arg.as_str() {
            "-d" | "--disable-error-context" => disable_error_context = true,
            "-v" | "--verbose" => verbose = true,
            "-l" | "--license" => {
                // Open the LICENSE file
                let license = include_str!("..\\LICENSE.md");

                // Trim the first two characters from each line
                let license = license
                    .lines()
                    .map(|line| &line[2..])
                    .collect::<Vec<&str>>()
                    .join("\n");

                println!("{}", license);
                std::process::exit(0);
            },
            "-i" | "--info" => {
                println!("RattleScript REPL Version: {} | Language Version: {}", repl::REPL_VERSION, env!("CARGO_PKG_VERSION"));
                println!("Author: Haven Selph <havenselph@gmail.com>");
                println!("Repository: <https://github.com/HavenSelph/rattlescript>");
                println!("MIT License: <https://choosealicense.com/licenses/mit/>");
                std::process::exit(0);
            }
            "-h" | "--help" => {
                print_help(&args[0]);
                std::process::exit(0);
            }
            arg => {
                // Check if first character is a dash
                if arg.starts_with('-') {
                    eprintln!("Unknown option: {}", arg);
                    print_help(&args[0]);
                    std::process::exit(1);
                }
                if filename.is_some() {
                    print_help(&args[0]);
                    std::process::exit(1);
                }
                filename = Some(arg);
            }
        }
    }

    if filename.is_none() {
        let mut repl = repl::Repl::new(verbose);
        repl.run();
        std::process::exit(0);
    }

    let filename = match filename {
        Some(filename) => filename,
        None => {
            eprintln!("No filename provided");
            std::process::exit(1);
        }
    };

    let result = run_file(filename, verbose);

    match result {
        Ok(_) => std::process::exit(0),
        Err(err) => {
            if disable_error_context {
                eprintln!("{} {}", err.span.0, err.message);
                std::process::exit(1);
            } else {
                err.print_with_source();
                std::process::exit(1);
            }
        }
    }
}
