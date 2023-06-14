use crate::ast::{ArgumentType, AST};
use crate::common::{make, Ref, Span};
use crate::error::{runtime_error as error, Result};
use crate::interpreter::{Interpreter, Scope};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Clone)]
pub struct IteratorValue(pub Ref<dyn Iterator<Item = Value>>);

struct RcChars {
    _rc: Rc<String>,
    chars: Option<std::str::Chars<'static>>,
}

impl Iterator for RcChars {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ref mut chars) = self.chars {
            Some(chars.next()?.to_string())
        } else {
            None
        }
    }
}

impl RcChars {
    pub fn from_rc(rc: Rc<String>) -> RcChars {
        let mut new = Self {
            _rc: rc,
            chars: None,
        };
        new.chars = Some(unsafe { &*Rc::as_ptr(&new._rc) }.chars());
        new
    }
}

struct StringIterator {
    data: RcChars,
}

impl Iterator for StringIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Value> {
        Some(Value::String(Rc::new(self.data.next()?)))
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

struct DictIterator {
    dict: Ref<HashMap<Value, Value>>,
    index: usize,
}

impl Iterator for DictIterator {
    type Item = Value;

    fn next(&mut self) -> Option<Value> {
        let dict = self.dict.borrow();
        if self.index >= dict.len() {
            None
        } else {
            let (key, _) = dict.iter().nth(self.index).unwrap();
            self.index += 1;
            Some(key.clone())
        }
    }
}

impl IteratorValue {
    pub fn for_string(data: Rc<String>) -> IteratorValue {
        IteratorValue(make!(StringIterator {
            data: RcChars::from_rc(data)
        }))
    }

    pub fn for_range(start: &i64, end: &i64) -> IteratorValue {
        IteratorValue(make!((*start..*end).map(Value::Integer)))
    }

    pub fn for_array(array: Ref<Vec<Value>>) -> IteratorValue {
        IteratorValue(make!(ArrayIterator { array, index: 0 }))
    }

    pub fn for_dict(dict: Ref<HashMap<Value, Value>>) -> IteratorValue {
        IteratorValue(make!(DictIterator { dict, index: 0 }))
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
    pub args: Vec<(String, Option<Value>, ArgumentType)>,
    pub required: usize,
    pub scope: Ref<Scope>,
}

pub struct ClassField {
    pub val: Value,
    pub is_static: bool,
}

pub struct Class {
    pub span: Span,
    pub name: String,
    pub parents: Option<Ref<Vec<Value>>>,
    pub fields: HashMap<String, (Value, bool)>,
}

pub struct ClassInstance {
    pub span: Span,
    pub name: String,
    pub parents: Option<Ref<Vec<Value>>>,
    pub in_initializer: bool,
    pub scope: Ref<Scope>,
}

impl ClassInstance {
    pub fn set_in_initializer(&mut self, in_initializer: bool) {
        self.in_initializer = in_initializer;
    }
}

pub type CallArgValues = Vec<(Option<String>, Value)>;
pub type BuiltInFunctionType = fn(&mut Interpreter, Ref<Scope>, &Span, Vec<Value>) -> Result<Value>;

macro_rules! builtin {
    ($name:ident) => {
        crate::interpreter::value::Value::BuiltInFunction(
            crate::interpreter::value::BuiltInFunction(
                stringify!($name),
                make!(crate::interpreter::builtin::$name),
            ),
        )
    };
}

pub(crate) use builtin;

#[derive(Clone)]
pub struct BuiltInFunction(pub &'static str, pub Ref<BuiltInFunctionType>);

#[derive(Clone)]
pub enum Value {
    ClassField(Ref<ClassField>),
    Array(Ref<Vec<Value>>),
    Tuple(Ref<Vec<Value>>),
    Boolean(bool),
    BuiltInFunction(BuiltInFunction),
    Float(f64),
    File(Ref<(String, std::fs::File)>),
    Class(Ref<Class>),
    ClassInstance(Ref<ClassInstance>),
    Function(Ref<Function>),
    Integer(i64),
    Iterator(IteratorValue),
    Nothing,
    Range(i64, i64),
    Dict(Ref<HashMap<Value, Value>>),
    String(Rc<String>),
    Namespace(Span, String, Ref<Scope>),
}

impl Value {
    pub fn new_class_field(val: Value, is_static: bool) -> Value {
        Value::ClassField(make!(ClassField { val, is_static }))
    }

    pub fn unpack_class_field(self) -> (Value, bool) {
        match self {
            Value::ClassField(field) => (field.borrow().val.clone(), field.borrow().is_static),
            _ => unreachable!("{} is not a class field", self.type_of()),
        }
    }

    pub fn class_instance_set_in_initializer(&mut self, in_initializer: bool) {
        match self {
            Value::ClassInstance(instance) => instance.borrow_mut().set_in_initializer(in_initializer),
            _ => unreachable!("{} is not a class instance", self.type_of()),
        }
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Value::Integer(num) => num.hash(state),
            Value::Float(num) => num.to_bits().hash(state),
            Value::String(string) => string.hash(state),
            Value::Boolean(boolean) => boolean.hash(state),
            Value::Nothing => 0.hash(state),
            Value::Iterator(_) => 0.hash(state),
            Value::Range(start, end) => {
                start.hash(state);
                end.hash(state);
            }
            Value::File(_) => 0.hash(state),
            Value::BuiltInFunction(name) => name.0.hash(state),
            Value::Function(func) => func.as_ptr().hash(state),
            Value::Class(class) => class.as_ptr().hash(state),
            Value::ClassInstance(instance) => instance.as_ptr().hash(state),
            Value::Array(array) => {
                for item in array.borrow().iter() {
                    item.hash(state);
                }
            }
            Value::Tuple(tuple) => tuple.borrow().iter().for_each(|item| item.hash(state)),
            Value::Dict(items) => items.borrow().iter().for_each(|item| item.hash(state)),
            _ => unreachable!("{} is not hashable", self.type_of()),
        }
    }
}

impl std::fmt::Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Integer(num) => write!(f, "{}", num),
            Value::Float(num) => write!(f, "{}", num),
            Value::String(string) => write!(f, "{}", string),
            Value::Boolean(boolean) => write!(f, "{}", boolean),
            Value::File(name) => write!(f, "<file {}>", name.borrow().0),
            Value::Nothing => write!(f, "nothing"),
            Value::Iterator(_) => write!(f, "<iterator>"),
            Value::Range(start, end) => write!(f, "{}..{}", start, end),
            Value::BuiltInFunction(name) => write!(f, "<builtin {}>", name.0),
            Value::Function(func) => {
                let func = func.borrow();
                write!(f, "<function {}: {}>", func.name, func.span.0)
            }
            Value::Class(class) => {
                let class = class.borrow();
                write!(f, "<class {}: {}>", class.name, class.span.0)
            }
            Value::ClassInstance(instance) => {
                let instance = instance.borrow();
                write!(f, "<class-instance {}>", instance.name)
            }
            Value::Array(array) => {
                write!(f, "[")?;
                for (i, item) in array.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item.repr())?;
                }
                write!(f, "]")
            }
            Value::Tuple(tuple) => {
                write!(f, "(")?;
                for (i, item) in tuple.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", item.repr())?;
                }
                write!(f, ")")
            }
            Value::Dict(dict, ..) => {
                write!(f, "{{")?;
                for (i, (key, value)) in dict.borrow().iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key.repr(), value.repr())?;
                }
                write!(f, "}}")
            }
            Value::ClassField(_) => unreachable!("Class fields should never be debugged"),
            Value::Namespace(_, name, _) => write!(f, "<namespace {}>", name),
        }
    }
}

#[allow(unused_qualifications)]
impl std::cmp::PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        //     Class(Ref<Class>),
        //     ClassInstance(Ref<ClassInstance>),
        //     Function(Ref<Function>),
        //     Iterator(IteratorValue),
        //     Nothing,
        //     Range(i64, i64),
        //     Dict(Ref<std::collections::HashMap<Value, Value>>),
        match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => *left == *right,
            (Value::Integer(left), Value::Float(right)) => *left as f64 == *right,
            (Value::Float(left), Value::Float(right)) => *left == *right,
            (Value::Float(left), Value::Integer(right)) => *left == *right as f64,
            (Value::String(left), Value::String(right)) => *left == *right,
            (Value::Boolean(left), Value::Boolean(right)) => *left == *right,
            (Value::Array(left), Value::Array(right))
            | (Value::Tuple(left), Value::Tuple(right)) => {
                let left = left.borrow();
                let right = right.borrow();
                if left.len() != right.len() {
                    false
                } else {
                    left.iter().zip(right.iter()).all(|(a, b)| a == b)
                }
            }
            (Value::BuiltInFunction(left), Value::BuiltInFunction(right)) => left.0 == right.0,
            (Value::Class(left), Value::Class(right)) => left.as_ptr() == right.as_ptr(),
            (Value::ClassInstance(left), Value::ClassInstance(right)) => {
                left.as_ptr() == right.as_ptr()
            }
            (Value::Function(left), Value::Function(right)) => left.as_ptr() == right.as_ptr(),
            (Value::Iterator(..), Value::Iterator(..)) => false,
            (Value::Range(left_start, left_end), Value::Range(right_start, right_end)) => {
                left_start == right_start && left_end == right_end
            }
            (Value::Nothing, Value::Nothing) => true,
            (Value::Dict(left, ..), Value::Dict(right, ..)) => {
                let left = left.borrow();
                let right = right.borrow();
                if left.len() != right.len() {
                    false
                } else {
                    left.iter().all(|(key, value)| {
                        if let Some(other_value) = right.get(key) {
                            value == other_value
                        } else {
                            false
                        }
                    })
                }
            }
            _ => false,
        }
    }
}

#[allow(unused_qualifications)]
impl std::cmp::Eq for Value {}

impl Value {
    pub fn plus(&self, other: &Value, span: &Span) -> Result<Value> {
        Ok(match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => Value::Integer(*left + *right),
            (Value::Integer(left), Value::Float(right)) => Value::Float(*left as f64 + *right),
            (Value::Float(left), Value::Float(right)) => Value::Float(*left + *right),
            (Value::Float(left), Value::Integer(right)) => Value::Float(*left + *right as f64),
            (Value::String(left), Value::String(right)) => {
                Value::String(Rc::new(left.to_string() + right.as_str()))
            }
            (Value::Array(left), Value::Array(right)) => {
                let mut left = left.borrow().clone();
                let right = right.borrow();
                left.extend(right.iter().cloned());
                Value::Array(make!(left))
            }
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
                Value::String(Rc::new(left.repeat(*right as usize)))
            }
            _ => error!(span, "Invalid types for multiplication"),
        })
    }

    pub fn modulo(&self, other: &Value, span: &Span) -> Result<Value> {
        Ok(match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => Value::Integer(*left % *right),
            (Value::Integer(left), Value::Float(right)) => Value::Float(*left as f64 % *right),
            (Value::Float(left), Value::Float(right)) => Value::Float(*left % *right),
            (Value::Float(left), Value::Integer(right)) => Value::Float(*left % *right as f64),
            _ => error!(span, "Invalid types for modulo"),
        })
    }

    pub fn power(&self, other: &Value, span: &Span) -> Result<Value> {
        Ok(match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => {
                Value::Integer(left.pow(*right as u32))
            }
            (Value::Integer(left), Value::Float(right)) => {
                Value::Float(f64::powf(*left as f64, *right))
            }
            (Value::Float(left), Value::Integer(right)) => Value::Float(left.powf(*right as f64)),
            (Value::Float(left), Value::Float(right)) => Value::Float(left.powf(*right)),
            _ => error!(span, "Invalid types for exponentiation"),
        })
    }

    pub fn divide(&self, other: &Value, span: &Span) -> Result<Value> {
        Ok(match (self, other) {
            (Value::Integer(left), Value::Integer(right)) => {
                if *right == 0 {
                    error!(span, "Division by zero")
                }
                Value::Float(*left as f64 / *right as f64)
            }
            (Value::Integer(left), Value::Float(right)) => {
                if *right == 0.0 {
                    error!(span, "Division by zero")
                }
                Value::Float(*left as f64 / *right)
            }
            (Value::Float(left), Value::Float(right)) => {
                if *right == 0.0 {
                    error!(span, "Division by zero")
                }
                Value::Float(*left / *right)
            }
            (Value::Float(left), Value::Integer(right)) => {
                if *right == 0 {
                    error!(span, "Division by zero")
                }
                Value::Float(*left / *right as f64)
            }
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

        fn get_slice_params(
            span: &Span,
            a: Value,
            b: Option<Value>,
            c: Value,
            default_b: i64,
        ) -> Result<(i64, i64, i64)> {
            match (a, b, c) {
                (Value::Integer(a), None, Value::Integer(c)) => Ok((a, default_b, c)),
                (Value::Integer(a), Some(Value::Integer(b)), Value::Integer(c)) => Ok((a, b, c)),
                _ => error!(span, "Slice indices must be integers"),
            }
        }

        match self {
            Value::String(s) => {
                let s = s;
                let (start, end, step) = get_slice_params(span, start, end, step, s.len() as i64)?;
                let res = s
                    .chars()
                    .take(end as usize)
                    .skip(start as usize)
                    .step_by(step as usize)
                    .collect::<String>();
                Ok(Value::String(Rc::new(res)))
            }
            Value::Array(a) => {
                let a = a.borrow();
                let (start, end, step) = get_slice_params(span, start, end, step, a.len() as i64)?;
                let res = a
                    .iter()
                    .take(end as usize)
                    .skip(start as usize)
                    .step_by(step as usize)
                    .cloned()
                    .collect::<Vec<_>>();
                Ok(Value::Array(make!(res)))
            }
            _ => error!(span, "Can only slice strings"),
        }
    }

    pub fn get_field(&self, span: &Span, field: &String) -> Result<Value> {
        Ok(match self {
            Value::Namespace(span, _, scope) => match scope.borrow().get(field) {
                Some(value) => value,
                None => {
                    error!(span, "Field '{}' not found on namespace", field);
                }
            }
            Value::Class(class) => match class.borrow().fields.get(field) {
                Some(value) => {
                    if !value.1 {
                        error!(span, "Cannot access non-static field '{}' on class", field);
                    }
                    value.0.clone()
                }
                None => {
                    error!(span, "Field '{}' not found on class", field);
                }
            }
            Value::ClassInstance(instance) => match instance.borrow().scope.borrow().get(field) {
                Some(value) => {
                    // Fields will always be hidden in a ClassField value type, need to unpack them
                    match value {
                        Value::ClassField(class_field) => {
                            if class_field.borrow().is_static {
                                error!(span, "Cannot access static field '{}' on instance", field);
                            }
                            class_field.borrow().val.clone()
                        },
                        _ => unreachable!("Class fields should always be wrapped in a ClassField value type"),
                    }
                },
                None => {
                    error!(span, "Field '{}' not found on class instance", field);
                }
            },
            Value::Array(_) => match field.as_str() {
                "len" => builtin!(len),
                "push" => builtin!(push),
                "pop" => builtin!(pop),
                "str" => builtin!(to_str),
                "iter" => builtin!(to_iter),
                "dbg" => builtin!(debug),
                _ => {
                    error!(span, "Field '{}' not found on array", field);
                }
            },
            Value::Tuple(_) => match field.as_str() {
                "len" => builtin!(len),
                "str" => builtin!(to_str),
                "iter" => builtin!(to_iter),
                "dbg" => builtin!(debug),
                _ => {
                    error!(span, "Field '{}' not found on tuple", field);
                }
            },
            Value::Float(_) => match field.as_str() {
                "int" => builtin!(to_int),
                "str" => builtin!(to_str),
                "dbg" => builtin!(debug),
                _ => {
                    error!(span, "Field '{}' not found on float", field);
                }
            },
            Value::Boolean(_) => match field.as_str() {
                "int" => builtin!(to_int),
                "str" => builtin!(to_str),
                "dbg" => builtin!(debug),
                _ => {
                    error!(span, "Field '{}' not found on boolean", field);
                }
            },
            Value::Integer(_) => match field.as_str() {
                "str" => builtin!(to_str),
                "float" => builtin!(to_float),
                "dbg" => builtin!(debug),
                _ => {
                    error!(span, "Field '{}' not found on integer", field);
                }
            },
            Value::String(_) => match field.as_str() {
                "len" => builtin!(len),
                "split" => builtin!(split),
                "int" => builtin!(to_int),
                "float" => builtin!(to_float),
                "iter" => builtin!(to_iter),
                "dbg" => builtin!(debug),
                "strip" => builtin!(strip),
                _ => {
                    error!(span, "Field '{}' not found on string", field);
                }
            },
            Value::Dict(_) => match field.as_str() {
                "len" => builtin!(len),
                "str" => builtin!(to_str),
                "get" => builtin!(dict_get),
                "keys" => builtin!(dict_keys),
                "values" => builtin!(dict_values),
                "items" => builtin!(dict_items),
                "dbg" => builtin!(debug),
                _ => {
                    error!(span, "Field '{}' not found on dict", field);
                }
            },
            Value::Iterator(_) => match field.as_str() {
                "join" => builtin!(join),
                "enumerate" => builtin!(iter_enumerate),
                "to_array" => builtin!(to_array),
                "map" => builtin!(map),
                _ => {
                    error!(span, "Field '{}' not found on iterator", field);
                }
            },
            Value::File(_) => match field.as_str() {
                "read" => builtin!(file_read),
                "write" => builtin!(file_write),
                _ => {
                    error!(span, "Field '{}' not found on file", field);
                }
            },
            Value::Nothing => match field.as_str() {
                "str" => builtin!(to_str),
                "dbg" => builtin!(debug),
                _ => {
                    error!(span, "Field '{}' not found on nothing", field);
                }
            },
            _ => {
                error!(
                    span,
                    "Cannot access field '{}' on type {}", field, self.type_of()
                );
            }
        })
    }

    pub fn negate(&self, span: &Span) -> Result<Value> {
        Ok(match self {
            Value::Integer(i) => Value::Integer(-*i),
            Value::Float(f) => Value::Float(-*f),
            _ => error!(span, "Invalid type for negation"),
        })
    }

    pub fn not(&self, span: &Span) -> Result<Value> {
        Ok(match self {
            Value::Boolean(b) => Value::Boolean(!*b),
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
            Value::Array(arr) | Value::Tuple(arr) => {
                Value::Iterator(IteratorValue::for_array(arr.clone()))
            }
            Value::Dict(dict) => Value::Iterator(IteratorValue::for_dict(dict.clone())),
            _ => error!(span, "Cannot iterate over this type"),
        })
    }

    #[allow(dead_code)]
    pub fn repr(&self) -> String {
        match self {
            Value::Integer(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::String(s) => escape_string(s.as_str()),
            Value::Boolean(b) => b.to_string(),
            Value::Iterator(_) => "<iterator>".to_string(),
            Value::Function(func) => {
                let func = func.borrow();
                format!("<function {}: {}>", func.name, func.span.0)
            }
            Value::Class(class) => {
                let class = class.borrow();
                format!("<class {}: {}>", class.name, class.span.0)
            }
            Value::ClassInstance(inst) => {
                let inst = inst.borrow();
                format!("<class-instance {}>", inst.name)
            }
            Value::File(file) => format!("<file {}>", file.borrow().0),
            Value::Range(start, end) => format!("{}..{}", start, end),
            Value::BuiltInFunction(name) => format!("<built-in function {}>", name.0),
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
            Value::Tuple(tup) => {
                let tup = tup.borrow();
                let mut s = "(".to_string();
                for (i, v) in tup.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&v.repr());
                }
                if tup.len() == 1 {
                    s.push(',');
                }
                s.push(')');
                s
            }
            Value::Dict(dict, ..) => {
                let dict = dict.borrow();
                let mut s = "{".to_string();
                for (i, (k, v)) in dict.iter().enumerate() {
                    if i > 0 {
                        s.push_str(", ");
                    }
                    s.push_str(&k.repr());
                    s.push_str(": ");
                    s.push_str(&v.repr());
                }
                s.push('}');
                s
            }
            Value::Namespace(..) => "<namespace>".to_string(),
            Value::ClassField(_) => {
                unreachable!("Class fields should never be printed");
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
            (Value::String(s), Value::Integer(index)) => match s.chars().nth(*index as usize) {
                Some(c) => Value::String(Rc::new(c.to_string())),
                None => error!(span, "Index out of bounds"),
            },
            (Value::Array(arr), Value::Integer(index)) => match arr.borrow().get(*index as usize) {
                Some(v) => v.clone(),
                None => error!(span, "Index out of bounds"),
            },
            (Value::Tuple(tup), Value::Integer(index)) => match tup.borrow().get(*index as usize) {
                Some(v) => v.clone(),
                None => error!(span, "Index out of bounds"),
            },
            (Value::Dict(dict, ..), key) => {
                if !key.is_hashable() {
                    error!(span, "Key must be hashable");
                }
                match dict.borrow().get(key) {
                    Some(v) => v.clone(),
                    None => error!(span, "Key not found"),
                }
            }
            (value, index) => error!(span, "Can't index {:?} with {:?}", value, index),
        })
    }

    pub fn set_index(&self, index: &Value, value: &Value, span: &Span) -> Result<()> {
        match (self, index) {
            (Value::Tuple(_), _) => error!(span, "Can't set index on tuple"),
            (Value::Array(arr), Value::Integer(index)) => {
                let mut arr = arr.borrow_mut();
                match arr.get_mut(*index as usize) {
                    Some(v) => {
                        *v = value.clone();
                    }
                    None => error!(span, "Index out of bounds"),
                }
            }
            (Value::Dict(dict, ..), key) => {
                if !key.is_hashable() {
                    error!(span, "Key must be hashable");
                }
                dict.borrow_mut().insert(key.clone(), value.clone());
            }
            (value, index) => error!(span, "Can't index {:?} with {:?}", value, index),
        }
        Ok(())
    }

    pub fn set_field(&self,  span: &Span, field: &str, value: &Value) -> Result<()> {
        match self {
            Value::Class(class) => {
                let mut class = class.borrow_mut();
                match class.fields.get(field) {
                    Some(class_field) => {
                        if !class_field.1 {
                            error!(span, "Field {} is not static", field);
                        }
                    },
                    None => error!(span, "Field {} not found", field),
                }
                class.fields.insert(field.to_string(), (value.clone(), true));
            }
            Value::ClassInstance(inst) => {
                // Find out if we are in new or not
                let create_new = inst.borrow().in_initializer;

                // Classes can now have static fields, so we need to check if the field is static
                match inst.borrow().scope.borrow().get(field) {
                    Some(value) => {
                        match value {
                            Value::ClassField { 0:class_field } => {
                                if class_field.borrow().is_static {
                                    error!(span, "Cannot set static field {} on instance", field);
                                }
                            },
                            _ => {
                                unreachable!("Class field should always be wrapped in ClassField")
                            }
                        }
                    },
                    None => if !create_new {
                        error!(span, "Field {} not found", field)
                    },
                };
                let inst = inst.borrow();
                let value = Value::new_class_field(value.clone(), false);
                inst.scope
                    .borrow_mut()
                    .insert(field, value, false, span)?;
            }
            _ => error!(span, "Can't set field on {:?}", self),
        }
        Ok(())
    }

    pub fn is_hashable(&self) -> bool {
        matches!(
            self,
            Value::Integer(..)
                | Value::Float(..)
                | Value::String(..)
                | Value::Boolean(..)
                | Value::Nothing
                | Value::Iterator(..)
                | Value::Range(..)
                | Value::File(..)
                | Value::BuiltInFunction(..)
                | Value::Function(..)
                | Value::Class(..)
                | Value::ClassInstance(..)
                | Value::Array(..)
                | Value::Tuple(..)
                | Value::Dict(..)
        )
    }

    pub fn type_of(&self) -> &str {
        match self {
            Value::Integer(..) => "Integer",
            Value::Float(..) => "Float",
            Value::String(..) => "String",
            Value::Boolean(..) => "Boolean",
            Value::Function(..) => "Function",
            Value::Class(..) => "Class",
            Value::ClassInstance(..) => "ClassInstance",
            Value::File(..) => "File",
            Value::Range(..) => "Range",
            Value::BuiltInFunction(..) => "BuiltInFunction",
            Value::Nothing => "Nothing",
            Value::Array(..) => "Array",
            Value::Tuple(..) => "Tuple",
            Value::Dict(..) => "Dict",
            Value::Iterator(..) => "Iterator",
            Value::Namespace(..) => "Namespace",
            Value::ClassField(..) => unreachable!("ClassField should never be printed"),
        }
    }

    // pub fn get_class_field(&self, field: &str) -> Option<(Value, bool)> {
    //     match self {
    //         Value::Class(class) => {
    //             let class = class.borrow();
    //             match class.fields.get(field) {
    //                 Some((value, is_static)) => Some((value.clone(), *is_static)),
    //                 None => unreachable!("Class field should always be found"),
    //             }
    //         }
    //         Value::ClassInstance(instance) => {
    //             let instance = instance.borrow();
    //             let temp = match instance.scope.borrow().get(field) {
    //                 Some(value) => {
    //                     match value {
    //                         Value::ClassField { 0:class_field } => {
    //                             Some((class_field.borrow().val.clone(), class_field.borrow().is_static))
    //                         },
    //                         _ => {
    //                             unreachable!("Class field should always be wrapped in ClassField")
    //                         }
    //                     }
    //                 },
    //                 None => None,
    //             }; temp
    //         }
    //         _ => unreachable!("Can't get class field on {:?}", self),
    //     }
    // }
}

fn escape_string(s: &str) -> String {
    let mut escaped = String::from('"');
    for c in s.chars() {
        match c {
            '"' => escaped.push_str("\\\""),
            '\\' => escaped.push_str("\\\\"),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            c => escaped.push(c),
        }
    }
    escaped.push('"');
    escaped
}
