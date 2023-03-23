use crate::lexer::Location;
use crate::utils::error;


#[derive(Debug)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String)
}


#[derive(Debug)]
pub enum AST {
    IntegerLiteral(Location, i64),
    FloatLiteral(Location, f64),
    StringLiteral(Location, String),
    Plus(Location, Box<AST>, Box<AST>),
    Minus(Location, Box<AST>, Box<AST>),
    Multiply(Location, Box<AST>, Box<AST>),
    Divide(Location, Box<AST>, Box<AST>),
}

impl AST {
    #[allow(dead_code)]
    pub fn location(&self) -> Location {
        match self {
            AST::IntegerLiteral(loc, _) => loc.clone(),
            AST::FloatLiteral(loc, _) => loc.clone(),
            AST::StringLiteral(loc, _) => loc.clone(),
            AST::Plus(loc, _, _) => loc.clone(),
            AST::Minus(loc, _, _) => loc.clone(),
            AST::Multiply(loc, _, _) => loc.clone(),
            AST::Divide(loc, _, _) => loc.clone(),
        }
    }

    pub fn run(&self) -> Value {
        match self {
            AST::IntegerLiteral(_, num) => Value::Integer(*num),
            AST::FloatLiteral(_, num) => Value::Float(*num),
            AST::StringLiteral(_, string) => Value::String(string.clone()),
            AST::Plus(loc, left, right) => {
                let left = left.run();
                let right = right.run();
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
                let left = left.run();
                let right = right.run();
                match (left, right) {
                    (Value::Integer(left), Value::Integer(right)) => Value::Integer(left - right),
                    (Value::Integer(left), Value::Float(right)) => Value::Float(left as f64 - right),
                    (Value::Float(left), Value::Float(right)) => Value::Float(left - right),
                    (Value::Float(left), Value::Integer(right)) => Value::Float(left - right as f64),
                    _ => error!("{loc}: Invalid types for subtraction")
                }
            },
            AST::Multiply(loc, left, right) => {
                let left = left.run();
                let right = right.run();
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
                let left = left.run();
                let right = right.run();
                match (left, right) {
                    (Value::Integer(left), Value::Integer(right)) => Value::Integer(left / right),
                    (Value::Integer(left), Value::Float(right)) => Value::Float(left as f64 / right),
                    (Value::Float(left), Value::Float(right)) => Value::Float(left / right),
                    (Value::Float(left), Value::Integer(right)) => Value::Float(left / right as f64),
                    _ => error!("{loc}: Invalid types for division")
                }
            },
        }
    }
}