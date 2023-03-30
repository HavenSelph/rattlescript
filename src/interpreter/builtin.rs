use crate::error::{runtime_error as error, Result};
use crate::interpreter::value::Value;
use crate::common::Span;

pub fn print(_span: &Span, args: Vec<Value>) -> Result<Value> {
    for (i, arg) in args.iter().enumerate() {
        if i != 0 {
            print!(" ");
        }
        print!("{}", arg.repr())
    }
    println!();
    Ok(Value::Nothing)
}

pub fn len(span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "len() takes exactly one argument");
    }

    Ok(match &args[0] {
        Value::String(string) => Value::Integer(string.borrow().len() as i64),
        Value::Array(array) => Value::Integer(array.borrow().len() as i64),
        Value::Range(start, end) => Value::Integer(end - start),
        other => error!(span, "len() does not support {:?}", other),
    })
}

pub fn exit(span: &Span, args: Vec<Value>) -> Result<Value> {
    let code = match args.get(0) {
        Some(val) => match val {
            Value::Integer(i) => *i,
            _ => error!(span, "exit() may only take an integer as argument"),
        }
        None => 0,
    };

    match code.try_into() {
        Ok(code) => std::process::exit(code),
        Err(_) => std::process::exit(1),
    }
}
