use std::sync::Arc;
use crate::token::Location;
use crate::ast::AST;
use crate::interpreter::{Scope, Ref};
use crate::utils::error;



#[derive(Debug, Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    String(String),
    Boolean(bool),
    BuiltInFunction(String),
    Function{body: Arc<AST>, args: Vec<String>, scope: Ref<Scope>},
    Nothing,
}

impl Value {
    pub fn plus(self, other: Value, loc: &Location) -> Value {
        match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => Value::Integer(left + right),
            (Value::Integer(left), Value::Float(right)) => Value::Float(left as f64 + right),
            (Value::Float(left), Value::Float(right)) => Value::Float(left + right),
            (Value::Float(left), Value::Integer(right)) => Value::Float(left + right as f64),
            (Value::String(left), Value::String(right)) => Value::String(left + &right),
            _ => error!(loc, "Invalid types for addition")
        }
    }

    pub fn minus(self, other: Value, loc: &Location) -> Value {
        match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => Value::Integer(left - right),
            (Value::Integer(left), Value::Float(right)) => Value::Float(left as f64 - right),
            (Value::Float(left), Value::Float(right)) => Value::Float(left - right),
            (Value::Float(left), Value::Integer(right)) => Value::Float(left - right as f64),
            _ => error!(loc, "Invalid types for subtraction")
        }
    }

    pub fn multiply(self, other: Value, loc: &Location) -> Value {
        match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => Value::Integer(left * right),
            (Value::Integer(left), Value::Float(right)) => Value::Float(left as f64 * right),
            (Value::Float(left), Value::Float(right)) => Value::Float(left * right),
            (Value::Float(left), Value::Integer(right)) => Value::Float(left * right as f64),
            (Value::String(left), Value::Integer(right)) => {
                if right < 0 { error!(loc, "{right} is not a positive integer.") }
                Value::String(left.repeat(right as usize))
            },
            _ => error!(loc, "Invalid types for multiplication")
        }
    }

    pub fn divide(self, other: Value, loc: &Location) -> Value {
        match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => Value::Integer(left / right),
            (Value::Integer(left), Value::Float(right)) => Value::Float(left as f64 / right),
            (Value::Float(left), Value::Float(right)) => Value::Float(left / right),
            (Value::Float(left), Value::Integer(right)) => Value::Float(left / right as f64),
            _ => error!(loc, "Invalid types for division")
        }
    }

    pub fn slice(self, start: Option<Value>, end: Option<Value>, step: Option<Value>, loc: &Location) -> Value {
        let start = start.unwrap_or(Value::Integer(0));
        let step = step.unwrap_or(Value::Integer(1));
        match self {
            Value::String(s) => {
                let end = end.unwrap_or(Value::Integer(s.len() as i64));
                match (start, end, step) {
                    (Value::Integer(start), Value::Integer(end), Value::Integer(step)) => {
                        if step == 0 { error!(loc, "Step cannot be 0") }
                        let mut result = String::new();
                        let mut i = start;
                        while i < end {
                            result.push(s.chars().nth(i as usize).unwrap());
                            i += step;
                        }
                        Value::String(result)
                    },
                    _ => error!(loc, "Invalid types for slice")
                }
            },
            _ => error!(loc, "Can only slice strings")
        }
    }


    pub fn not(self, loc: &Location) -> Value {
        match self {
            Value::Boolean(b) => Value::Boolean(!b),
            _ => error!(loc, "Invalid type for not")
        }
    }
    pub fn and(self, other: Value, loc: &Location) -> Value {
        match (self, other) {
            (Value::Boolean(left), Value::Boolean(right)) => Value::Boolean(left && right),
            _ => error!(loc, "Invalid types for and")
        }
    }
    pub fn or(self, other: Value, loc: &Location) -> Value {
        match (self, other) {
            (Value::Boolean(left), Value::Boolean(right)) => Value::Boolean(left || right),
            _ => error!(loc, "Invalid types for or")
        }
    }

    pub fn equals(self, other: Value, _loc: &Location) -> Value {
        match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => Value::Boolean(left == right),
            (Value::Integer(left), Value::Float(right)) => Value::Boolean(left as f64 == right),
            (Value::Float(left), Value::Float(right)) => Value::Boolean(left == right),
            (Value::Float(left), Value::Integer(right)) => Value::Boolean(left == right as f64),
            (Value::String(left), Value::String(right)) => Value::Boolean(left == right),
            (Value::Boolean(left), Value::Boolean(right)) => Value::Boolean(left == right),
            _ => Value::Boolean(false)
        }
    }
    pub fn not_equals(self, other: Value, loc: &Location) -> Value {
        match self.equals(other, loc) {
            Value::Boolean(b) => Value::Boolean(!b),
            _ => unreachable!("equals should always return a boolean")
        }
    }
    pub fn less_than(self, other: Value, loc: &Location) -> Value {
        match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => Value::Boolean(left < right),
            (Value::Integer(left), Value::Float(right)) => Value::Boolean((left as f64) < right),
            (Value::Float(left), Value::Float(right)) => Value::Boolean(left < right),
            (Value::Float(left), Value::Integer(right)) => Value::Boolean(left < right as f64),
            (Value::String(left), Value::String(right)) => Value::Boolean(left < right),
            _ => error!(loc, "Invalid types for less than")
        }
    }

    pub fn greater_than(self, other: Value, loc: &Location) -> Value {
        other.less_than(self, loc)
    }
    pub fn less_than_equals(self, other: Value, loc: &Location) -> Value {
        match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => Value::Boolean(left <= right),
            (Value::Integer(left), Value::Float(right)) => Value::Boolean((left as f64) <= right),
            (Value::Float(left), Value::Float(right)) => Value::Boolean(left <= right),
            (Value::Float(left), Value::Integer(right)) => Value::Boolean(left <= right as f64),
            (Value::String(left), Value::String(right)) => Value::Boolean(left <= right),
            _ => error!(loc, "Invalid types for less than")
        }
    }
    pub fn greater_than_equals(self, other: Value, loc: &Location) -> Value {
        other.less_than_equals(self, loc)
    }

}