use crate::ast::AST;
use crate::common::{Ref, get, make, Span};
use crate::error::{Result, runtime_error as error};
use crate::interpreter::Scope;
use std::rc::Rc;

#[derive(Clone)]
pub struct IteratorValue(pub Ref<dyn Iterator<Item = Value>>);

struct StringIterator {
    string: Ref<String>,
    index: usize,
}

impl Iterator for StringIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Value> {
        let string = self.string.borrow();
        if self.index >= string.len() {
            None
        } else {
            let c = string.chars().nth(self.index).unwrap();
            self.index += 1;
            Some(Value::String(make!(c.to_string())))
        }
    }
}

struct ArrayIterator {
    array: Ref<Vec<Value>>,
    index: usize,
}

impl Iterator for ArrayIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Value> {
        let array = self.array.borrow();
        if self.index >= array.len() {
            None
        } else {
            let item = array[self.index].clone();
            self.index += 1;
            Some(item)
        }
    }
}

impl IteratorValue {
    pub fn for_string(string: Ref<String>) -> IteratorValue {
        IteratorValue(make!(StringIterator { string, index: 0 }))
    }

    pub fn for_range(start: &i64, end: &i64) -> IteratorValue {
        IteratorValue(make!((*start..*end).map(Value::Integer)))
    }

    pub fn for_array(array: Ref<Vec<Value>>) -> IteratorValue {
        IteratorValue(make!(ArrayIterator { array, index: 0 }))
    }
}

impl std::fmt::Debug for IteratorValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<iterator>")
    }
}

pub struct Function {
    pub span: Span,
    pub name: String,
    pub body: Rc<AST>,
    pub args: Vec<String>,
    pub scope: Ref<Scope>,
}

#[derive(Clone)]
pub enum Value {
    Integer(i64),
    Float(f64),
    Boolean(bool),
    String(Ref<String>),
    BuiltInFunction(Ref<String>),
    Function(Ref<Function>),
    Iterator(IteratorValue),
    Range(i64, i64),
    Array(Ref<Vec<Value>>),
    Nothing,
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(num) => write!(f, "{}", num),
            Value::Float(num) => write!(f, "{}", num),
            Value::String(string) => write!(f, "{}", string.borrow()),
            Value::Boolean(boolean) => write!(f, "{}", boolean),
            Value::Nothing => write!(f, "nothing"),
            Value::Iterator(_) => write!(f, "<iterator>"),
            Value::Range(start, end) => write!(f, "{}..{}", start, end),
            Value::BuiltInFunction(name) => write!(f, "<builtin {}>", name.borrow()),
            Value::Function(func) => {
                let func = func.borrow();
                write!(f, "<function {}: {}>", func.name, func.span.0)
            }
            Value::Array(array) => {
                write!(f, "[")?;
                for (i, item) in array.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", item)?;
                }
                write!(f, "]")
            }
        }
    }
}

impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => *left == *right,
            (Value::Integer(left), Value::Float(right)) => *left as f64 == *right,
            (Value::Float(left), Value::Float(right)) => *left == *right,
            (Value::Float(left), Value::Integer(right)) => *left == *right as f64,
            (Value::String(left), Value::String(right)) => *left.borrow() == *right.borrow(),
            (Value::Boolean(left), Value::Boolean(right)) => *left == *right,
            (Value::Array(left), Value::Array(right)) => {
                let left = left.borrow();
                let right = right.borrow();
                if left.len() != right.len() {
                    false
                } else {
                    left.iter().zip(right.iter()).all(|(a, b)| a == b)
                }
            },
            _ => false,
        }
    }
}

impl Value {
    pub fn plus(&self, other: &Value, span: &Span) -> Result<Value> {
        Ok(match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => Value::Integer(*left + *right),
            (Value::Integer(left), Value::Float(right)) => Value::Float(*left as f64 + *right),
            (Value::Float(left), Value::Float(right)) => Value::Float(*left + *right),
            (Value::Float(left), Value::Integer(right)) => Value::Float(*left + *right as f64),
            (Value::String(left), Value::String(right)) => Value::String(make!(left.borrow().clone() + get!(right))),
            _ => error!(span, "Invalid types for addition"),
        })
    }

    pub fn minus(&self, other: &Value, span: &Span) -> Result<Value> {
        Ok(match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => Value::Integer(*left - *right),
            (Value::Integer(left), Value::Float(right)) => Value::Float(*left as f64 - *right),
            (Value::Float(left), Value::Float(right)) => Value::Float(*left - *right),
            (Value::Float(left), Value::Integer(right)) => Value::Float(*left - *right as f64),
            _ => error!(span, "Invalid types for subtraction"),
        })
    }

    pub fn multiply(&self, other: &Value, span: &Span) -> Result<Value> {
        Ok(match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => Value::Integer(*left * *right),
            (Value::Integer(left), Value::Float(right)) => Value::Float(*left as f64 * *right),
            (Value::Float(left), Value::Float(right)) => Value::Float(*left * *right),
            (Value::Float(left), Value::Integer(right)) => Value::Float(*left * *right as f64),
            (Value::String(left), Value::Integer(right)) => {
                if *right < 0 {
                    error!(span, "{right} is not a positive integer.")
                }
                Value::String(make!(left.borrow().repeat(*right as usize)))
            }
            _ => error!(span, "Invalid types for multiplication"),
        })
    }

    pub fn divide(&self, other: &Value, span: &Span) -> Result<Value> {
        Ok(match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => Value::Integer(*left / *right),
            (Value::Integer(left), Value::Float(right)) => Value::Float(*left as f64 / *right),
            (Value::Float(left), Value::Float(right)) => Value::Float(*left / *right),
            (Value::Float(left), Value::Integer(right)) => Value::Float(*left / *right as f64),
            _ => error!(span, "Invalid types for division"),
        })
    }

    pub fn slice(
        &self,
        start: Option<Value>,
        end: Option<Value>,
        step: Option<Value>,
        span: &Span,
    ) -> Result<Value> {

        let start = start.unwrap_or_else(|| Value::Integer(0));
        let step = step.unwrap_or_else(|| Value::Integer(1));

        fn get_slice_params(span: &Span, a: Value, b: Option<Value>, c: Value, default_b: i64) -> Result<(i64, i64, i64)> {
            match (a, b, c) {
                (Value::Integer(a), None, Value::Integer(c)) => Ok((a, default_b, c)),
                (Value::Integer(a), Some(Value::Integer(b)), Value::Integer(c)) => Ok((a, b, c)),
                _ => error!(span, "Slice indices must be integers"),
            }
        }

        match self {
            Value::String(s) => {
                let s = s.borrow();
                let (start, end, step) = get_slice_params(span, start, end, step, s.len() as i64)?;
                let res = s.chars().take(end as usize).skip(start as usize).step_by(step as usize).collect::<String>();
                Ok(Value::String(make!(res)))
            },
            Value::Array(a) => {
                let a = a.borrow();
                let (start, end, step) = get_slice_params(span, start, end, step, a.len() as i64)?;
                let res = a.iter().take(end as usize).skip(start as usize).step_by(step as usize).cloned().collect::<Vec<_>>();
                Ok(Value::Array(make!(res)))
            },
            _ => error!(span, "Can only slice strings"),
        }
    }

    pub fn not(&self, span: &Span) -> Result<Value> {
        Ok(match self {
            Value::Boolean(b) => Value::Boolean(!b),
            _ => error!(span, "Invalid type for not"),
        })
    }
    pub fn and(&self, other: &Value, span: &Span) -> Result<Value> {
        Ok(match (self, other) {
            (Value::Boolean(left), Value::Boolean(right)) => Value::Boolean(*left && *right),
            _ => error!(span, "Invalid types for and"),
        })
    }
    pub fn or(&self, other: &Value, span: &Span) -> Result<Value> {
        Ok(match (self, other) {
            (Value::Boolean(left), Value::Boolean(right)) => Value::Boolean(*left || *right),
            _ => error!(span, "Invalid types for or"),
        })
    }

    pub fn equals(&self, other: &Value, _: &Span) -> Result<Value> {
        Ok(Value::Boolean(self == other))
    }
    pub fn not_equals(&self, other: &Value, _: &Span) -> Result<Value> {
        Ok(Value::Boolean(self != other))
    }
    pub fn less_than(&self, other: &Value, span: &Span) -> Result<Value> {
        Ok(match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => Value::Boolean(*left < *right),
            (Value::Integer(left), Value::Float(right)) => Value::Boolean((*left as f64) < *right),
            (Value::Float(left), Value::Float(right)) => Value::Boolean(*left < *right),
            (Value::Float(left), Value::Integer(right)) => Value::Boolean(*left < *right as f64),
            (Value::String(left), Value::String(right)) => Value::Boolean(*left < *right),
            _ => error!(span, "Invalid types for less than"),
        })
    }

    pub fn greater_than(&self, other: &Value, span: &Span) -> Result<Value> {
        other.less_than(self, span)
    }

    pub fn less_equals(&self, other: &Value, span: &Span) -> Result<Value> {
        Ok(match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => Value::Boolean(*left <= *right),
            (Value::Integer(left), Value::Float(right)) => Value::Boolean((*left as f64) <= *right),
            (Value::Float(left), Value::Float(right)) => Value::Boolean(*left <= *right),
            (Value::Float(left), Value::Integer(right)) => Value::Boolean(*left <= *right as f64),
            (Value::String(left), Value::String(right)) => Value::Boolean(*left <= *right),
            _ => error!(span, "Invalid types for less than"),
        })
    }

    pub fn greater_equals(&self, other: &Value, span: &Span) -> Result<Value> {
        other.less_equals(self, span)
    }

    pub fn iterator(&self, span: &Span) -> Result<Value> {
        Ok(match self {
            Value::String(s) => Value::Iterator(IteratorValue::for_string(s.clone())),
            Value::Range(start, end) => Value::Iterator(IteratorValue::for_range(start, end)),
            Value::Array(arr) => Value::Iterator(IteratorValue::for_array(arr.clone())),
            _ => error!(span, "Cannot iterate over this type"),
        })
    }

    #[allow(dead_code)]
    pub fn repr(&self) -> String {
        match self {
            Value::Integer(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::String(s) => format!("\"{}\"", s.borrow()),
            Value::Boolean(b) => b.to_string(),
            Value::Iterator(_) => "<iterator>".to_string(),
            Value::Function(func) => {
                let func = func.borrow();
                format!("<function {}: {}>", func.name, func.span.0)
            }
            Value::Range(start, end) => format!("{}..{}", start, end),
            Value::BuiltInFunction(name) => format!("<built-in function {}>", name.borrow()),
            Value::Nothing => "nothing".to_string(),
            Value::Array(arr) => {
                let arr = arr.borrow();
                let mut s = "[".to_string();
                for (i, v) in arr.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&v.repr());
                }
                s.push(']');
                s
            }
        }
    }

    pub fn create_range(start: &Value, end: &Value, span: &Span) -> Result<Value> {
        Ok(match (start, end) {
            (Value::Integer(start), Value::Integer(end)) => Value::Range(*start, *end),
            _ => error!(span, "Must be integers for range"),
        })
    }

    pub fn index(&self, index: &Value, span: &Span) -> Result<Value> {
        Ok(match (self, index) {
            (Value::String(s), Value::Integer(index)) => {
                match s.borrow().chars().nth(*index as usize) {
                    Some(c) => Value::String(make!(c.to_string())),
                    None => error!(span, "Index out of bounds"),
                }
            }
            (Value::Array(arr), Value::Integer(index)) => {
                match arr.borrow().get(*index as usize) {
                    Some(v) => v.clone(),
                    None => error!(span, "Index out of bounds"),
                }
            }
            (value, index) => error!(span, "Can't index {:?} with {:?}", value, index),
        })
    }
}