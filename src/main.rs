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

const LICENSE: &str = "MIT License\n\nCopyright (c) 2023 Haven Alexander Selph-Pfister\n\nPermission is hereby granted, free of charge, to any person obtaining a copy\nof this software and associated documentation files (the \"Software\"), to deal\nin the Software without restriction, including without limitation the rights\nto use, copy, modify, merge, publish, distribute, sublicense, and/or sell\ncopies of the Software, and to permit persons to whom the Software is\nfurnished to do so, subject to the following conditions:\n\nThe above copyright notice and this permission notice shall be included in all\ncopies or substantial portions of the Software.\n\nTHE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR\nIMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,\nFITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE\nAUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER\nLIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,\nOUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE\nSOFTWARE.\n\n[Learn what this license permits you to do](https://choosealicense.com/licenses/mit/)\n";

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
                println!("{}", LICENSE);
                std::process::exit(0);
            }
            "-i" | "--info" => {
                println!(
                    "RattleScript REPL Version: {} | Language Version: {}",
                    repl::REPL_VERSION,
                    env!("CARGO_PKG_VERSION")
                );
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
