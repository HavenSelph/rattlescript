use crate::error::{Result, Error};
use crate::interpreter::{Ref, Scope, Interpreter};
use crate::ast::AST;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;
use std::sync::Mutex;
use crate::value::Value;

pub struct Repl {
    interpreter: Interpreter,
    global_scope: Ref<Scope>,
}

impl Repl {
    pub fn new() -> Repl {
        let interpreter = Interpreter::new();
        let global_scope = Rc::new(Mutex::new(Scope {
            vars: HashMap::new(),
            parent: None,
            in_function: false,
        }));
        Repl {
            interpreter,
            global_scope
        }
    }

    fn run_once(&mut self) -> Result<()> {
        let mut input = String::new();
        let ast = loop {
            let mut temp = String::new();
            print!("{}", if input.is_empty() { ">>> " } else { "... " });
            std::io::stdout().flush().expect("Failed to flush stdout");
            std::io::stdin().read_line(&mut temp).expect("Failed to read line");
            if temp.trim().is_empty() {
                break self.try_parse(input.clone())?;
            }
            input.push_str(&temp);
            match self.try_parse(input.clone()) {
                Ok(ast) => break ast,
                Err(Error::UnexpectedEOF(..)) => continue,
                Err(e) => return Err(e),
            }
        };
        let val = self.interpreter.run_block_without_scope(&ast, self.global_scope.clone())?;
        match val {
            Value::Nothing => {},
            _ => println!("{}", val.repr()),
        }
        Ok(())
    }

    fn try_parse(&self, input: String) -> Result<Rc<AST>> {
        let mut lex = crate::lexer::Lexer::new(input, "<repl>".to_string());
        let tokens = lex.lex()?;
        let mut parser = crate::parser::Parser::new(tokens);
        let ast = parser.parse()?;
        Ok(ast)
    }

    pub fn run(&mut self) {
        loop {
            match self.run_once() {
                Ok(_) => {},
                Err(e) => {
                    println!("{:?}", e);
                }
            }
        }
    }
}
