/*
    Copyright (C) 2023  Haven Selph
    Copyright (C) 2023  Mustafa Quraish
    Check the LICENSE file for more information.
 */

use crate::common::{make, Ref, Span};
use crate::error::{runtime_error as error, Result};
use crate::interpreter::value::{CallArgValues, Value};
use crate::interpreter::{Interpreter, Scope};
use crate::interpreter::random::RandomState;
use std::io::{Read, Write};
use std::rc::Rc;

pub fn print(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    _span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    for (i, arg) in args.iter().enumerate() {
        if i != 0 {
            print!(" ");
        }
        print!("{:?}", arg)
    }
    println!();
    Ok(Value::Nothing)
}

pub fn repr(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "repr() takes exactly one argument");
    }
    Ok(Value::String(Rc::new(args[0].repr())))
}

pub fn len(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "len() takes exactly one argument");
    }

    Ok(match &args[0] {
        Value::String(string) => Value::Integer(string.len() as i64),
        Value::Array(array) | Value::Tuple(array) => Value::Integer(array.borrow().len() as i64),
        Value::Dict(dict) => Value::Integer(dict.borrow().len() as i64),
        Value::Range(start, end) => Value::Integer(end - start),
        other => error!(span, "len() does not support {:?}", other),
    })
}

pub fn dump(
    _interpreter: &mut Interpreter,
    scope: Ref<Scope>,
    _span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    // Dump the scope
    if !args.is_empty() {
        error!(_span, "dump() takes no arguments");
    }
    println!("{:#?}", scope.borrow().parent);
    Ok(Value::Nothing)
}

pub fn push(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 2 {
        error!(span, "push() takes exactly two arguments");
    }
    match &args[0] {
        Value::Array(array) => {
            array.borrow_mut().push(args[1].clone());
            Ok(Value::Nothing)
        }
        other => error!(span, "push() does not support {:?}", other),
    }
}

pub fn pop(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "pop() takes exactly one argument");
    }
    match &args[0] {
        Value::Array(array) => {
            let mut array = array.borrow_mut();
            if array.is_empty() {
                error!(span, "pop() called on empty array");
            }
            Ok(array.pop().unwrap())
        }
        other => error!(span, "pop() does not support {:?}", other),
    }
}

pub fn exit(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    let code = match args.get(0) {
        Some(val) => match val {
            Value::Integer(i) => *i,
            _ => error!(span, "exit() may only take an integer as argument"),
        },
        None => 0,
    };

    match code.try_into() {
        Ok(code) => std::process::exit(code),
        Err(_) => std::process::exit(1),
    }
}

pub fn input(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    let prompt = if args.len() == 1 {
        match &args[0] {
            Value::String(string) => string.to_string(),
            _ => error!(span, "input() may only take a string as argument"),
        }
    } else if args.is_empty() {
        String::new()
    } else {
        error!(span, "input() takes either one or no arguments");
    };
    let mut input = String::new();
    print!("{}", prompt);
    std::io::Write::flush(&mut std::io::stdout()).unwrap();
    std::io::stdin().read_line(&mut input).unwrap();
    input = input.trim_end().to_string();
    Ok(Value::String(Rc::new(input)))
}

pub fn dict_get(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() < 2 || args.len() > 3 {
        error!(span, "dict_get() takes two or three arguments");
    }
    let dict = match &args[0] {
        Value::Dict(dict) => dict,
        _ => error!(span, "dict_get() may only take a dict as first argument"),
    };
    let key = &args[1];
    let default = if args.len() == 3 {
        &args[2]
    } else {
        &Value::Nothing
    };
    let dict = dict.borrow();
    match dict.get(key) {
        Some(value) => Ok(value.clone()),
        None => Ok(default.clone()),
    }
}

pub fn dict_items(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "dict_items() takes exactly one argument");
    }
    let dict = match &args[0] {
        Value::Dict(dict) => dict,
        _ => error!(span, "dict_items() may only take a dict as argument"),
    };
    let dict = dict.borrow();
    let mut items = Vec::new();
    for (key, value) in dict.iter() {
        items.push(Value::Tuple(make!(vec![key.clone(), value.clone()])));
    }
    Ok(Value::Array(make!(items)))
}


pub fn dict_keys(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "dict_keys() takes exactly one argument");
    }
    let dict = match &args[0] {
        Value::Dict(dict) => dict,
        _ => error!(span, "dict_keys() may only take a dict as argument"),
    };
    let dict = dict.borrow();
    let mut keys = Vec::new();
    for key in dict.keys() {
        keys.push(key.clone());
    }
    Ok(Value::Array(make!(keys)))
}

pub fn dict_values(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "dict_values() takes exactly one argument");
    }
    let dict = match &args[0] {
        Value::Dict(dict) => dict,
        _ => error!(span, "dict_values() may only take a dict as argument"),
    };
    let dict = dict.borrow();
    let mut values = Vec::new();
    for value in dict.values() {
        values.push(value.clone());
    }
    Ok(Value::Array(make!(values)))
}

pub fn split(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 2 {
        error!(span, "split() takes exactly two arguments");
    }
    let string = match &args[0] {
        Value::String(string) => string,
        _ => error!(span, "split() may only take a string as first argument"),
    };
    println!("{}", string);
    let separator = match &args[1] {
        Value::String(string) => string,
        _ => error!(span, "split() may only take a string as second argument"),
    };
    let mut items = Vec::new();
    for item in string.split(separator.as_str()) {
        items.push(Value::String(Rc::new(item.to_string())));
    }
    Ok(Value::Array(make!(items)))
}

pub fn strip(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() > 2 {
        error!(span, "strip() takes at most two arguments");
    }
    let string = match &args[0] {
        Value::String(string) => string.to_string(),
        _ => error!(span, "strip() may only take a string as first argument"),
    };
    let chars = if args.len() == 2 {
        match &args[1] {
            Value::String(string) => string.to_string(),
            _ => error!(span, "strip() may only take a string as second argument"),
        }
    } else {
        " \t\n\r".to_string()
    };

    Ok(Value::String(Rc::new(
        string.trim_matches(|c| chars.contains(c)).to_string(),
    )))
}

pub fn lower(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "lower() takes exactly one argument");
    }
    let string = match &args[0] {
        Value::String(string) => string.to_string(),
        _ => error!(span, "lower() may only take a string as argument"),
    };
    Ok(Value::String(Rc::new(string.to_lowercase())))
}

pub fn upper(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "upper() takes exactly one argument");
    }
    let string = match &args[0] {
        Value::String(string) => string.to_string(),
        _ => error!(span, "upper() may only take a string as argument"),
    };
    Ok(Value::String(Rc::new(string.to_uppercase())))
}

pub fn join(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 2 {
        error!(span, "join() takes exactly two arguments");
    }
    let iter = match &args[0] {
        Value::Iterator(iter) => iter,
        _ => error!(
            span,
            "join() may only take an iterable as the first argument"
        ),
    };

    let separator = match args.get(1) {
        Some(Value::String(string)) => string.to_string(),
        None => " ".to_string(),
        _ => error!(span, "join() may only take a string as the second argument"),
    };
    let mut result = String::new();
    let iter = &mut *(*iter.0).borrow_mut();
    for (i, item) in iter.enumerate() {
        if i > 0 {
            result.push_str(separator.as_str());
        }
        match item {
            Value::String(string) => result.push_str(string.as_str()),
            _ => error!(span, "join() may only take an iterator of strings"),
        }
    }
    Ok(Value::String(Rc::new(result)))
}

pub fn map(
    interpreter: &mut Interpreter,
    scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 2 {
        error!(span, "map() takes exactly two arguments");
    }
    let iter = match &args[0] {
        Value::Iterator(iter) => iter,
        _ => error!(
            span,
            "map() may only take an iterable as the first argument"
        ),
    };
    let function = &args[1];
    match function {
        Value::Function(_) => {}
        Value::BuiltInFunction(_) => {}
        _ => error!(
            span,
            "map() may only take a function as the second argument"
        ),
    };
    let mut result = Vec::new();
    let iter = &mut *(*iter.0).borrow_mut();
    for item in iter {
        let args: CallArgValues = vec![(None, item.clone())];
        result.push(interpreter.do_call(span, scope.clone(), None, function.clone(), &args)?);
    }
    Value::Array(make!(result)).iterator(span)
}

pub fn to_int(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "int() takes exactly one argument");
    }
    let value = match &args[0] {
        Value::Integer(i) => *i,
        Value::Float(f) => *f as i64,
        Value::Boolean(b) => *b as i64,
        Value::String(string) => match string.parse() {
            Ok(i) => i,
            Err(_) => error!(span, "Could not parse string as integer"),
        },
        _ => error!(span, "int() does not support {:?}", args[0]),
    };
    Ok(Value::Integer(value))
}

pub fn to_float(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "float() takes exactly one argument");
    }
    let value = match &args[0] {
        Value::Integer(i) => *i as f64,
        Value::Float(f) => *f,
        Value::String(string) => match string.parse() {
            Ok(f) => f,
            Err(_) => error!(span, "Could not parse string as float"),
        },
        _ => error!(span, "float() does not support {:?}", args[0]),
    };
    Ok(Value::Float(value))
}

pub fn to_str(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "str() takes exactly one argument");
    }
    let value = format!("{:?}", args[0]);
    Ok(Value::String(Rc::new(value)))
}

pub fn to_iter(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "iter() takes exactly one argument");
    }
    args[0].iterator(span)
}

pub fn to_array(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "collect() takes exactly one argument");
    }
    let iter = match &args[0] {
        Value::Iterator(iter) => iter,
        _ => error!(span, "collect() may only take an iterable as argument"),
    };
    let iter = &mut *(*iter.0).borrow_mut();
    let mut items = Vec::new();
    for item in iter {
        items.push(item.clone());
    }
    Ok(Value::Array(make!(items)))
}

pub fn iter_enumerate(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "enumerate() takes exactly one argument");
    }
    let iter = match &args[0] {
        Value::Iterator(iter) => iter,
        _ => error!(span, "enumerate() may only take an iterable as argument"),
    };
    let iter = &mut *(*iter.0).borrow_mut();
    let mut items = Vec::new();
    for (i, item) in iter.enumerate() {
        items.push(Value::Tuple(make!(vec![Value::Integer(i as i64), item])));
    }
    Ok(Value::Array(make!(items)))
}

pub fn file_open(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "open() takes exactly one argument");
    }
    let path = match &args[0] {
        Value::String(string) => string.clone(),
        _ => error!(span, "open() may only take a string as first argument"),
    };
    let file = match std::fs::File::open(path.to_string()) {
        Ok(file) => file,
        Err(err) => error!(span, "Could not open file: {}", err),
    };
    Ok(Value::File(make!((path.to_string(), file))))
}

pub fn file_read(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "read() takes exactly one argument");
    }
    let file = match &args[0] {
        Value::File(file) => file,
        _ => error!(span, "read() may only take a file as first argument"),
    };
    let mut file = file.borrow_mut();
    let mut buffer = String::new();
    match file.1.read_to_string(&mut buffer) {
        Ok(_) => Ok(Value::String(Rc::new(buffer))),
        Err(err) => error!(span, "Could not read file: {}", err),
    }
}

pub fn file_write(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 2 {
        error!(span, "write() takes exactly two arguments");
    }
    let file = match &args[0] {
        Value::File(file) => file,
        _ => error!(span, "write() may only take a file as first argument"),
    };
    let string = match &args[1] {
        Value::String(string) => string,
        _ => error!(span, "write() may only take a string as second argument"),
    };
    let mut file = file.borrow_mut();
    match file.1.write_all(string.as_bytes()) {
        Ok(_) => Ok(Value::Nothing),
        Err(err) => error!(span, "Could not write to file: {}", err),
    }
}

pub fn debug(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "debug() takes exactly one argument");
    }
    println!("{:?}", args[0]);
    Ok(args[0].clone())
}

pub fn new_random_state(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 0 {
        error!(span, "rand() takes no arguments");
    }
    Ok(Value::RandomState(make!(RandomState::new())))
}

pub fn randf(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "randf() takes at most one argument");
    }
    let state = match args[0] {
        Value::RandomState(ref state) => state,
        _ => error!(span, "randf() may only take a random state as argument"),
    };
    let random = state.borrow_mut().next();
    Ok(Value::Float(random as f64 / (2.0f64).powi(64)))
}

pub fn randi(
    _interpreter: &mut Interpreter,
    _scope: Ref<Scope>,
    span: &Span,
    args: Vec<Value>,
) -> Result<Value> {
    let state = match args.get(0) {
        Some(Value::RandomState(ref state)) => state,
        _ => error!(span, "randi() requires a random state as first argument"),
    };
    let (min, max) = match args.len() {
        1 => (0, 2),
        2 => match args[1] {
            Value::Integer(min) => (0, min),
            _ => error!(span, "randi() may only take an integer as argument"),
        },
        3 => match (&args[1], &args[2]) {
            (Value::Integer(min), Value::Integer(max)) => (*min, *max),
            _ => error!(span, "randi() may only take integers as arguments"),
        },
        _ => error!(span, "randi() takes at most two arguments"),
    };
    let random = state.borrow_mut().next() as i64;
    Ok(Value::Integer(min + if random > 0 { random } else { -random } % (max - min)))
}
