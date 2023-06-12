#![allow(clippy::upper_case_acronyms)]

use crate::error::Result;

mod ast;
mod common;
mod compiler;
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

fn compile_file(filename: &str, out_filename: &str) -> Result<()> {
    use std::io::Write;

    let content = std::fs::read_to_string(filename).expect("Couldn't open input file");

    let mut lex = lexer::Lexer::new(content, Box::leak(filename.to_string().into_boxed_str()));
    let tokens = lex.lex()?;

    let mut parser = parser::Parser::new(tokens);
    let ast = parser.parse()?;

    let mut compiler = compiler::Compiler::new();
    let code = compiler.compile(&ast)?;

    // output code to out.c
    let mut out_file = std::fs::File::create(out_filename).expect("Couldn't open output file");
    out_file
        .write_all(code.as_bytes())
        .expect("Couldn't write to output file");
    // println!("{}", code);
    Ok(())
}

fn print_help(filename: &str) {
    println!("Usage: {} [options] [filename]", filename);
    println!("Options:");
    println!("  -c, --compile                 Compile the input file (default: false)");
    println!("  -d, --disable-error-context   Disable error context (default: false)");
    println!("  -v, --verbose                 Enable verbose output (default: false)");
    println!("  -h, --help                    Print this help message");
}

fn main() {
    let args = std::env::args().collect::<Vec<String>>();

    let mut filename = None;
    let out_filename = "./local/compiled/out.c";
    let mut compile = false;
    let mut disable_error_context = false;
    let mut verbose = false;

    for arg in args.iter().skip(1) {
        match arg.as_str() {
            "-c" | "--compile" => compile = true,
            "-d" | "--disable-error-context" => disable_error_context = true,
            "-v" | "--verbose" => verbose = true,
            "-h" | "--help" => {
                print_help(&args[0]);
                std::process::exit(0);
            }
            arg => {
                if filename.is_some() {
                    print_help(&args[0]);
                    std::process::exit(1);
                }
                filename = Some(arg);
            }
        }
    }

    if filename.is_none() && !compile {
        let mut repl = repl::Repl::new(verbose);
        repl.run();
        std::process::exit(0);
    } else if compile && filename.is_none() {
        eprintln!("No filename provided");
        std::process::exit(1);
    }

    let filename = match filename {
        Some(filename) => filename,
        None => {
            eprintln!("No filename provided");
            std::process::exit(1);
        }
    };

    let result = if compile {
        compile_file(filename, out_filename)
    } else {
        run_file(filename, verbose)
    };

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
