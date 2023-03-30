use crate::ast::AST;
use crate::common::Ref;
use crate::error::{Error, Result, ErrorKind};
use crate::interpreter::value::Value;
use crate::interpreter::{Interpreter, Scope};
use std::io::Write;
use std::rc::Rc;

pub struct Repl {
    interpreter: Interpreter,
    global_scope: Ref<Scope>,
}

impl Repl {
    pub fn new() -> Repl {
        let interpreter = Interpreter::new();
        let global_scope = Scope::new(None, false);
        Repl {
            interpreter,
            global_scope,
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
                Err(Error{kind: ErrorKind::UnexpectedEOF, ..}) => {}
                Err(err) => return Err(err),
            }
        };
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
        let mut parser = crate::parser::Parser::new(tokens);
        parser.parse()
    }

    pub fn run(&mut self) {
        loop {
            match self.run_once() {
                Ok(_) => {}
                Err(err) => {
                    if err.span.0.line == err.span.1.line {
                        let len = err.span.1.column - err.span.0.column;
                        if len <= 1 {
                            println!("   {}\x1b[0;31m▲\x1b[0m", " ".repeat(err.span.0.column));
                        } else {
                            println!("   {}\x1b[0;31m└{}┘\x1b[0m", " ".repeat(err.span.0.column), "─".repeat(len - 2));
                        }
                    }
                    println!("\x1b[0;31m{}\x1b[0m", err);
                }
            }
        }
    }
}
