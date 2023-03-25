use crate::token::Location;
use crate::utils::{runtime_error as error, Result};
use crate::value::Value;

pub fn print(_loc: &Location, args: Vec<Value>) -> Result<Value> {
    for (i, arg) in args.iter().enumerate() {
        if i != 0 {
            print!(" ");
        }
        match arg {
            Value::Integer(num) => print!("{}", num),
            Value::Float(num) => print!("{}", num),
            Value::String(string) => print!("{}", string),
            Value::Boolean(boolean) => print!("{}", boolean),
            Value::Nothing => print!("nothing"),
            Value::Iterator(_) => print!("<iterator>"),
            Value::Range(start, end) => print!("{}..{}", start, end),
            _ => print!("{:?}", arg),
        }
    }
    println!();
    Ok(Value::Nothing)
}

pub fn len(loc: &Location, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(loc, "len() takes exactly one argument");
    }

    Ok(match &args[0] {
        Value::String(string) => Value::Integer(string.len() as i64),
        other => error!(loc, "len() does not support {:?}", other),
    })
}
