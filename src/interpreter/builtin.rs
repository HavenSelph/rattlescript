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

pub fn to_iter(span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "iter() takes exactly one argument");
    }
    args[0].iterator(span)
}

pub fn to_array(span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "array() takes exactly one argument");
    }
    let iter = match &args[0] {
        Value::Iterator(iter) => iter,
        _ => error!(span, "array() may only take an iterable as argument"),
    };
    let iter = &mut *(*iter.0).borrow_mut();
    let items = iter.collect();
    Ok(Value::Array(make!(items)))
}

pub fn iter_enumerate(span: &Span, args: Vec<Value>) -> Result<Value> {
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

pub fn file_open(span: &Span, args: Vec<Value>) -> Result<Value> {
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

pub fn file_read(span: &Span, args: Vec<Value>) -> Result<Value> {
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

pub fn file_write(span: &Span, args: Vec<Value>) -> Result<Value> {
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

pub fn debug(span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "debug() takes exactly one argument");
    }
    println!("{:?}", args[0]);
    Ok(args[0].clone())
}

struct RandomState {
    x: i32,
    y: i32,
    z: i32,
    w: i32,
}

static mut STATE: RandomState = RandomState {
    x: 123456789,
    y: 362436069,
    z: 521288629,
    w: 88675123,
};

fn rand() -> i32 {
    unsafe {
        let t = STATE.x ^ (STATE.x << 11);
        STATE.x = STATE.y;
        STATE.y = STATE.z;
        STATE.z = STATE.w;
        STATE.w = STATE.w ^ (STATE.w >> 19) ^ (t ^ (t >> 8));
        STATE.w
    }
}

pub fn random(span: &Span, args: Vec<Value>) -> Result<Value> {
    if !args.is_empty() {
        error!(span, "random() takes no arguments");
    }
    Ok(Value::Float(rand() as f64 / i32::MAX as f64))
}

pub fn random_int(span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 2 {
        error!(span, "random_int() takes exactly two arguments");
    }
    let min = match &args[0] {
        Value::Integer(i) => *i,
        _ => error!(
            span,
            "random_int() may only take an integer as first argument"
        ),
    };
    let max = match &args[1] {
        Value::Integer(i) => *i,
        _ => error!(
            span,
            "random_int() may only take an integer as second argument"
        ),
    };
    let range = max - min;
    Ok(Value::Integer(min + rand() as i64 % range))
}

pub fn random_choice(span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "choice() takes exactly one argument");
    }
    let array = match &args[0] {
        Value::Array(array) => array,
        Value::Tuple(array) => array,
        _ => error!(
            span,
            "choice() may only take an array or tuple as first argument"
        ),
    };
    let array = &*array.borrow();
    if array.is_empty() {
        error!(span, "choice() may not take an empty array or tuple");
    }
    let index = rand() as usize % array.len();
    Ok(array[index].clone())
}

pub fn time_now(span: &Span, args: Vec<Value>) -> Result<Value> {
    if !args.is_empty() {
        error!(span, "now() takes no arguments");
    }
    let now = std::time::SystemTime::now();
    let since_the_epoch = now.duration_since(std::time::UNIX_EPOCH).unwrap();
    Ok(Value::Float(since_the_epoch.as_secs_f64()))
}

pub fn time_sleep(span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "sleep() takes exactly one argument");
    }
    let seconds = match &args[0] {
        Value::Float(f) => *f,
        _ => error!(span, "sleep() may only take a float as first argument"),
    };
    std::thread::sleep(std::time::Duration::from_secs_f64(seconds));
    Ok(Value::Nothing)
}

pub fn func_name(span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "name() takes exactly one argument");
    }
    let func = match &args[0] {
        Value::Function(func) => func.borrow().name.clone(),
        Value::BuiltInFunction(func) => func.0.to_string(),
        _ => error!(span, "name() may only take a function as first argument"),
    };
    Ok(Value::String(make!(func)))
}

pub fn func_args(span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "args() takes exactly one argument");
    }
    let func = match &args[0] {
        Value::Function(func) => func,
        Value::BuiltInFunction(_) => return Ok(Value::Array(make!(Vec::new()))),
        _ => error!(span, "args() may only take a function as first argument"),
    };
    let func = func.borrow();
    let mut array = Vec::new();
    for arg in &func.args {
        array.push(Value::String(make!(arg.0.clone())));
    }
    Ok(Value::Array(make!(array)))
}

pub fn func_location(span: &Span, args: Vec<Value>) -> Result<Value> {
    if args.len() != 1 {
        error!(span, "loc() takes exactly one argument");
    }
    let loc = match &args[0] {
        Value::Function(func) => func.borrow().span.0,
        Value::BuiltInFunction(_) => crate::common::Location {
            filename: "<builtin>",
            line: 0,
            column: 0
        },
        _ => error!(span, "loc() may only take a function as first argument"),
    };
    let array = vec![
        Value::String(make!(loc.filename.to_string())),
        Value::Integer(loc.line as i64),
        Value::Integer(loc.column as i64),
    ];
    Ok(Value::Tuple(make!(array)))
}
