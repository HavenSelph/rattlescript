/*
    Rattlescript is a dynamically typed, interpreted programming language written in Rust.
    Copyright (C) 2023  Haven Selph

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

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

const LICENSE: &str = include_str!("../LICENSE.md");

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
                println!("GNU General Public License v3.0: <https://www.gnu.org/licenses/>");
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

    let rattle_script_path = match std::env::var("RATTLESCRIPT_PATH") {
        Ok(path) => {
            // Check if path is a directory
            if !std::path::Path::new(&path).is_dir() {
                eprintln!("{} is not a directory, consider set RATTLESCRIPT_PATH environment variable to the path of the RattleScript repository.", path);
                std::process::exit(1);
            }
            std::path::Path::new(&path).to_path_buf()
        }
        Err(_) => match std::env::current_dir() {
            Ok(path) => path,
            Err(_) => {
                eprintln!("Couldn't get current directory, set RATTLESCRIPT_PATH environment variable to the path of the RattleScript repository.");
                std::process::exit(1);
            }
        },
    };

    if rattle_script_path.join("std").exists() {
        std::env::set_var("RATTLESCRIPT_PATH", rattle_script_path.join("std"));
    } else {
        eprintln!("Couldn't find std directory in RATTLESCRIPT_PATH, set RATTLESCRIPT_PATH environment variable to the path of the RattleScript repository.");
        std::process::exit(1);
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
