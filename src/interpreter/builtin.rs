use std::io::Read;
use crate::common::{make, Span};
use crate::error::{runtime_error as error, Result};
use crate::interpreter::value::Value;

pub fn print(_span: &Span, args: Vec<Value>) -> Result<Value> {
    for (i, arg) in args.iter().enumerate() {
        if i != 0 {
            print!(" ");
        }
        print!("{:?}", arg)
    }
    println!();
    Ok(Value::Nothing)
}

pub fn repr(span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "repr() takes exactly one argument");
    }
    Ok(Value::String(make!(args[0].repr())))
}

pub fn len(span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "len() takes exactly one argument");
    }

    Ok(match &args[0] {
        Value::String(string) => Value::Integer(string.borrow().len() as i64),
        Value::Array(array) | Value::Tuple(array) => Value::Integer(array.borrow().len() as i64),
        Value::Dict(dict) => Value::Integer(dict.borrow().len() as i64),
        Value::Range(start, end) => Value::Integer(end - start),
        other => error!(span, "len() does not support {:?}", other),
    })
}

pub fn exit(span: &Span, args: Vec<Value>) -> Result<Value> {
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

pub fn input(span: &Span, args: Vec<Value>) -> Result<Value> {
    let prompt = if args.len() == 1 {
        match &args[0] {
            Value::String(string) => string.borrow().clone(),
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
    Ok(Value::String(make!(input)))
}

pub fn read_file(span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "read_file() takes exactly one argument");
    }
    let path = match &args[0] {
        Value::String(string) => string.borrow(),
        _ => error!(span, "read_file() may only take a string as argument"),
    };
    let mut file = match std::fs::File::open(path.as_str()) {
        Ok(file) => file,
        Err(e) => error!(span, "Could not open file {}: {}", path, e),
    };
    let mut contents = String::new();
    match file.read_to_string(&mut contents) {
        Ok(_) => {}
        Err(e) => error!(span, "Could not read file {}: {}", path, e),
    }
    Ok(Value::String(make!(contents)))
}

pub fn to_int(span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "int() takes exactly one argument");
    }
    let value = match &args[0] {
        Value::Integer(i) => *i,
        Value::Float(f) => *f as i64,
        Value::Boolean(b) => *b as i64,
        Value::String(string) => match string.borrow().parse() {
            Ok(i) => i,
            Err(_) => error!(span, "Could not parse string as integer"),
        },
        _ => error!(span, "int() does not support {:?}", args[0]),
    };
    Ok(Value::Integer(value))
}

pub fn to_float(span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "float() takes exactly one argument");
    }
    let value = match &args[0] {
        Value::Integer(i) => *i as f64,
        Value::Float(f) => *f,
        Value::String(string) => match string.borrow().parse() {
            Ok(f) => f,
            Err(_) => error!(span, "Could not parse string as float"),
        },
        _ => error!(span, "float() does not support {:?}", args[0]),
    };
    Ok(Value::Float(value))
}

pub fn to_str(span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "str() takes exactly one argument");
    }
    let value = format!("{:?}", args[0]);
    Ok(Value::String(make!(value)))
}
