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

use crate::ast::AST;
use crate::common::Ref;
use crate::error::{Error, ErrorKind, Result};
use crate::interpreter::value::Value;
use crate::interpreter::{Interpreter, Scope};
use std::io::Write;
use std::rc::Rc;

pub const REPL_VERSION: &str = "1.0.0";

pub struct Repl {
    interpreter: Interpreter,
    global_scope: Ref<Scope>,
    verbose: bool,
}

impl Repl {
    pub fn new(verbose: bool) -> Repl {
        let interpreter = Interpreter::new();
        let global_scope = Scope::new(None, false);
        Repl {
            interpreter,
            global_scope,
            verbose,
        }
    }

    fn run_once(&mut self) -> Result<()> {
        let mut input = String::new();
        let ast = loop {
            let mut temp = String::new();
            print!("{}", if input.is_empty() { ">>> " } else { "... " });
            std::io::stdout().flush().expect("Failed to flush stdout");
            std::io::stdin()
                .read_line(&mut temp)
                .expect("Failed to read line");
            if temp.trim().is_empty() {
                if input.trim().is_empty() {
                    return Ok(());
                }
                continue;
            }

            input.push_str(&temp);
            match self.try_parse(input.clone()) {
                Ok(ast) => break ast,
                Err(Error {
                    kind: ErrorKind::UnexpectedEOF,
                    ..
                }) => {}
                Err(err) => return Err(err),
            }
        };

        if self.verbose {
            println!("{:#?}", ast);
        }

        let val = self
            .interpreter
            .run_block_without_new_scope(&ast, self.global_scope.clone())?;
        match &val {
            Value::Nothing => {}
            _ => println!("{}", val.repr()),
        }
        Ok(())
    }

    fn try_parse(&self, input: String) -> Result<Rc<AST>> {
        let mut lex = crate::lexer::Lexer::new(input, "<repl>");
        let tokens = lex.lex()?;

        if self.verbose {
            for token in &tokens {
                println!("{:?}", token);
            }
        }

        let mut parser = crate::parser::Parser::new(tokens);
        parser.parse()
    }

    pub fn run(&mut self) {
        println!("Rattlescript  Copyright (C) 2023  Haven Selph\nThis program comes with ABSOLUTELY NO WARRANTY.\nThis is free software, and you are welcome to redistribute it\nunder certain conditions.\n");
        println!(
            "RattleScript REPL, press Ctrl-C to exit | REPL Version: {} | Language Version: {}",
            REPL_VERSION,
            env!("CARGO_PKG_VERSION")
        );
        println!("GNU General Public License v3.0: <https://www.gnu.org/licenses/>");
        loop {
            match self.run_once() {
                Ok(_) => {}
                Err(err) => {
                    if err.span.0.line == err.span.1.line {
                        let len = err.span.1.column - err.span.0.column;
                        if len <= 1 {
                            println!("   {}\x1b[0;31m▲\x1b[0m", " ".repeat(err.span.0.column));
                        } else {
                            println!(
                                "   {}\x1b[0;31m└{}┘\x1b[0m",
                                " ".repeat(err.span.0.column),
                                "─".repeat(len - 2)
                            );
                        }
                    }
                    println!("\x1b[0;31m{}\x1b[0m", err);
                }
            }
        }
    }
}
