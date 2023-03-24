use crate::ast::AST;
use crate::token::Location;
use crate::utils::error;
use crate::builtins;
use std::collections::HashMap;


#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    BuiltInFunction(String),
    None,
}

type BuiltInFunctionType = fn(&Location, Vec<Value>) -> Value;

pub struct Interpreter {
    vars: HashMap<String, Value>,
    builtins: HashMap<String, BuiltInFunctionType>,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut builtins = HashMap::new();
        builtins.insert("print".to_string(), builtins::print as BuiltInFunctionType);
        builtins.insert("len".to_string(), builtins::len as BuiltInFunctionType);

        Interpreter {
            vars: HashMap::new(),
            builtins
        }
    }

    pub fn run(&mut self, ast: &Box<AST>) -> Value {
        match ast.as_ref() {
            AST::Call(loc, func, args) => {
                let func = self.run(func);
                let args: Vec<_> = args.iter().map(|arg| self.run(arg)).collect();

                match &func {
                    Value::BuiltInFunction(func) => {
                        match self.builtins.get(func) {
                            Some(func) => func(loc, args),
                            None => unreachable!("{loc}: Built-in function {:?} not found", func)
                        }
                    }
                    _ => error!("{loc}: Can't call object {:?}", func)
                }
            },
            AST::Block(_, stmts) => {
                let mut last = Value::None;
                for stmt in stmts {
                    last = self.run(stmt);
                }
                last
            },
            AST::IntegerLiteral(_, num) => Value::Integer(*num),
            AST::FloatLiteral(_, num) => Value::Float(*num),
            AST::StringLiteral(_, string) => Value::String(string.clone()),
            AST::VarDeclaration(_, name, value) => {
                let value = self.run(value);
                self.vars.insert(name.clone(), value.clone());
                value
            },
            AST::Index(loc, left, right) => {
                let left = self.run(left);
                let right = self.run(right);
                match (&left, &right) {
                    (Value::String(left), Value::Integer(right)) => {
                        match left.chars().nth(*right as usize) {
                            Some(c) => Value::String(c.to_string()),
                            None => error!("{loc}: Index out of bounds")
                        }
                    },
                    _ => error!("{loc}: Can't index {:?} with {:?}", left, right)
                }
            },
            AST::Variable(loc, name) => {
                if let Some(_) = self.builtins.get(name) {
                    return Value::BuiltInFunction(name.clone())
                }
                if let Some(value) = self.vars.get(name) {
                    return value.clone()
                }
                error!("{loc}: Variable {} not found", name)
            },
            AST::Plus(loc, left, right) => {
                let left = self.run(left);
                let right = self.run(right);
                match (left, right) {
                    (Value::Integer(left), Value::Integer(right)) => Value::Integer(left + right),
                    (Value::Integer(left), Value::Float(right)) => Value::Float(left as f64 + right),
                    (Value::Float(left), Value::Float(right)) => Value::Float(left + right),
                    (Value::Float(left), Value::Integer(right)) => Value::Float(left + right as f64),
                    (Value::String(left), Value::String(right)) => Value::String(left + &right),
                    _ => error!("{loc}: Invalid types for addition")
                }
            },
            AST::Minus(loc, left, right) => {
                let left = self.run(left);
                let right = self.run(right);
                match (left, right) {
                    (Value::Integer(left), Value::Integer(right)) => Value::Integer(left - right),
                    (Value::Integer(left), Value::Float(right)) => Value::Float(left as f64 - right),
                    (Value::Float(left), Value::Float(right)) => Value::Float(left - right),
                    (Value::Float(left), Value::Integer(right)) => Value::Float(left - right as f64),
                    _ => error!("{loc}: Invalid types for subtraction")
                }
            },
            AST::Multiply(loc, left, right) => {
                let left = self.run(left);
                let right = self.run(right);
                match (left, right) {
                    (Value::Integer(left), Value::Integer(right)) => Value::Integer(left * right),
                    (Value::Integer(left), Value::Float(right)) => Value::Float(left as f64 * right),
                    (Value::Float(left), Value::Float(right)) => Value::Float(left * right),
                    (Value::Float(left), Value::Integer(right)) => Value::Float(left * right as f64),
                    (Value::String(left), Value::Integer(right)) => {
                        if right < 0 { error!("{loc}: {right} is not a positive integer.") }
                        Value::String(left.repeat(right as usize))
                    }
                    _ => error!("{loc}: Invalid types for multiplication")
                }
            },
            AST::Divide(loc, left, right) => {
                let left = self.run(left);
                let right = self.run(right);
                match (left, right) {
                    (Value::Integer(left), Value::Integer(right)) => Value::Integer(left / right),
                    (Value::Integer(left), Value::Float(right)) => Value::Float(left as f64 / right),
                    (Value::Float(left), Value::Float(right)) => Value::Float(left / right),
                    (Value::Float(left), Value::Integer(right)) => Value::Float(left / right as f64),
                    _ => error!("{loc}: Invalid types for division")
                }
            },
            AST::Slice {loc, lhs, start, end, step} => {
                let lhs = self.run(lhs);
                match lhs {
                    Value::String(s) => {
                        let start = if let Some(start) = start { self.run(start) } else { Value::Integer(0) };
                        let end = if let Some(end) = end { self.run(end) } else { Value::Integer(s.len() as i64) };
                        let step = if let Some(step) = step { self.run(step) } else { Value::Integer(1) };
                        match (start, end, step) {
                            (Value::Integer(start), Value::Integer(end), Value::Integer(step)) => {
                                if step == 0 { error!("{loc}: Step cannot be 0") }
                                let mut result = String::new();
                                let mut i = start;
                                while i < end {
                                    result.push(s.chars().nth(i as usize).unwrap());
                                    i += step;
                                }
                                Value::String(result)
                            },
                            _ => error!("{loc}: Invalid types for slice")
                        }
                    },
                    _ => error!("{loc}: Can only slice strings")
                }
            }
        }
    }
}
