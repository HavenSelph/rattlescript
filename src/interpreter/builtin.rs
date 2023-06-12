use std::io::{Read,Write};
use crate::common::{make, Span, Ref};
use crate::error::{runtime_error as error, Result};
use crate::interpreter::value::{Value, ClassInstance};
use crate::interpreter::{Interpreter, Scope, ControlFlow};
use std::collections::HashMap;


pub fn handle_call(interpreter: &mut Interpreter, scope: Ref<Scope>, span: &Span, callee: Value, args: Vec<Value>) -> Result<Value> {
    Ok(match callee {
        Value::Function(func) => {
            let new_scope = Scope::new(Some(func.borrow().scope.clone()), true);
            if args.len() > func.borrow().args.len() {
                error!(
                    span,
                    "Function expected no more than {} arguments, got {}",
                    func.borrow().args.len(),
                    args.len()
                )
            } else {
                let func = func.borrow();
                for (i, (name, default)) in func.args.iter().enumerate() {
                    if i < args.len() {
                        new_scope.borrow_mut().insert(name, args[i].clone(), false, span)?;
                    } else if let Some(default) = default {
                        new_scope
                            .borrow_mut()
                            .insert(name, default.clone(), false, span)?;
                    } else {
                        error!(
                            span,
                            "Function argument {} is required, but not provided", name
                        )
                    }
                }
            }
            let body = func.borrow().body.clone();
            interpreter.run(&body, new_scope)?;
            let value = if let ControlFlow::Return(value) = &interpreter.control_flow {
                value.clone()
            } else {
                Value::Nothing
            };
            interpreter.control_flow = ControlFlow::None;
            value
        }
        Value::BuiltInFunction(func) => {
            func.1.borrow()(interpreter, scope, span, args)?
        }
        Value::Class(class) => {
            let _class = class.borrow();
            let mut fields: HashMap<String, Value> = HashMap::new();
            for method in _class.methods.iter() {
                fields.insert(method.0.clone(), method.1.clone());
            }
            if args.len() > _class.fields.len() {
                error!(
                    span,
                    "Class expected no more than {:?} arguments, got {:?}",
                    _class.fields.len(),
                    args.len()
                );
            }
            for (i, (name, default)) in _class.fields.iter().enumerate() {
                if i < args.len() {
                    let arg = &args[i];
                    fields.insert(name.clone(), arg.clone());
                } else if let Some(default) = default {
                    fields.insert(name.clone(), default.clone());
                } else {
                    error!(
                        span,
                        "Class argument {} is required, but not provided", name
                    );
                }
            }
            Value::ClassInstance(make!(ClassInstance {
                class: class.clone(),
                fields
            }))
        }
        x => error!(span, "Can't call object {:?}", x),
    })
}


pub fn print(_interpreter: &mut Interpreter, _scope: Ref<Scope>, _span: &Span, args: Vec<Value>) -> Result<Value> {
    for (i, arg) in args.iter().enumerate() {
        if i != 0 {
            print!(" ");
        }
        print!("{:?}", arg)
    }
    println!();
    Ok(Value::Nothing)
}

pub fn repr(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "repr() takes exactly one argument");
    }
    Ok(Value::String(make!(args[0].repr())))
}

pub fn len(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
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

pub fn push(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
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

pub fn pop(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
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


pub fn exit(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
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

pub fn input(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
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

pub fn dict_get(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 2 {
        error!(span, "dict_get() takes exactly two arguments");
    }
    let dict = match &args[0] {
        Value::Dict(dict) => dict,
        _ => error!(span, "dict_get() may only take a dict as first argument"),
    };
    let key = &args[1];
    let dict = dict.borrow();
    match dict.get(key) {
        Some(value) => Ok(value.clone()),
        None => Ok(Value::Nothing),
    }
}

pub fn dict_items(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
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

pub fn dict_keys(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
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

pub fn dict_values(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
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

pub fn split(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 2 {
        error!(span, "split() takes exactly two arguments");
    }
    let string = match &args[0] {
        Value::String(string) => string.borrow(),
        _ => error!(span, "split() may only take a string as first argument"),
    };
    let separator = match &args[1] {
        Value::String(string) => string.borrow(),
        _ => error!(span, "split() may only take a string as second argument"),
    };
    let mut items = Vec::new();
    for item in string.split(separator.as_str()) {
        items.push(Value::String(make!(item.to_string())));
    }
    Ok(Value::Array(make!(items)))
}

pub fn join(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 2 {
        error!(span, "join() takes exactly two arguments");
    }
    let iter = match &args[0] {
        Value::Iterator(iter) => iter,
        _ => error!(span, "join() may only take an iterable as the first argument"),
    };

    let separator = match args.get(1) {
        Some(Value::String(string)) => string.borrow().clone(),
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
            Value::String(string) => result.push_str(string.borrow().as_str()),
            _ => error!(span, "join() may only take an iterator of strings"),
        }
    }
    Ok(Value::String(make!(result)))
}

pub fn map(interpreter: &mut Interpreter, scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 2 {
        error!(span, "map() takes exactly two arguments");
    }
    let iter = match &args[0] {
        Value::Iterator(iter) => iter,
        _ => error!(span, "map() may only take an iterable as the first argument"),
    };
    let function = &args[1];
    match function {
        Value::Function(_) => {},
        Value::BuiltInFunction(_) => {},
        _ => error!(span, "map() may only take a function as the second argument"),
    };
    let mut result = Vec::new();
    let iter = &mut *(*iter.0).borrow_mut();
    for item in iter {
        let value = handle_call(interpreter, scope.clone(), span, function.clone(), vec![item.clone()])?;
        result.push(value);
    }
    Value::Array(make!(result)).iterator(span)
}


pub fn to_int(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
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

pub fn to_float(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
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

pub fn to_str(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "str() takes exactly one argument");
    }
    let value = format!("{:?}", args[0]);
    Ok(Value::String(make!(value)))
}

pub fn to_iter(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "iter() takes exactly one argument");
    }
    args[0].iterator(span)
}

pub fn to_array(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
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

pub fn iter_enumerate(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
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

pub fn file_open(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "open() takes exactly one argument");
    }
    let path = match &args[0] {
        Value::String(string) => string.borrow().clone(),
        _ => error!(span, "open() may only take a string as first argument"),
    };
    let file = match std::fs::File::open(&path) {
        Ok(file) => file,
        Err(err) => error!(span, "Could not open file: {}", err),
    };
    Ok(Value::File(make!((path, file))))
}

pub fn file_read(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
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
        Ok(_) => Ok(Value::String(make!(buffer))),
        Err(err) => error!(span, "Could not read file: {}", err),
    }
}

pub fn file_write(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 2 {
        error!(span, "write() takes exactly two arguments");
    }
    let file = match &args[0] {
        Value::File(file) => file,
        _ => error!(span, "write() may only take a file as first argument"),
    };
    let string = match &args[1] {
        Value::String(string) => string.borrow(),
        _ => error!(span, "write() may only take a string as second argument"),
    };
    let mut file = file.borrow_mut();
    match file.1.write_all(string.as_bytes()) {
        Ok(_) => Ok(Value::Nothing),
        Err(err) => error!(span, "Could not write to file: {}", err),
    }
}

pub fn debug(_interpreter: &mut Interpreter, _scope: Ref<Scope>, span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "debug() takes exactly one argument");
    }
    println!("{:?}", args[0]);
    Ok(args[0].clone())
}
