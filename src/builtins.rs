use crate::interpreter::Value;
use crate::token::Location;
use crate::utils::error;

pub fn print(_loc: &Location, args: Vec<Value>) -> Value {
    for (i, arg) in args.iter().enumerate() {
        if i != 0 {
            print!(" ");
        }
        match arg {
            Value::Integer(num) => print!("{}", num),
            Value::Float(num) => print!("{}", num),
            Value::String(string) => print!("{}", string),
            Value::None => print!("None"),
            _ => print!("{:?}", arg)
        }
    }
    println!();
    Value::None
}

pub fn len(loc: &Location, args: Vec<Value>) -> Value {
    if args.len() != 1 {
        error!("{loc}: len() takes exactly one argument");
    }

    match &args[0] {
        Value::String(string) => Value::Integer(string.len() as i64),
        other => error!("{loc}: len() does not support {:?}", other),
    }
}