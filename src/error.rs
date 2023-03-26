use crate::token::Location;

#[allow(dead_code)]
#[derive(Debug)]
pub enum Error {
    Lexer(Location, String),
    Parser(Location, String),
    UnexpectedEOF(Location, String),
    Runtime(Location, String),
    Other(String),
}

pub type Result<T> = std::result::Result<T, Error>;

macro_rules! lexer_error {
    ($loc:expr, $($arg:tt)*) => {
        {
            crate::error::show_file_context($loc.clone(), format!($($arg)*));
            return Err(crate::error::Error::Lexer($loc.clone(), format!($($arg)*)))
        }
    }
}
pub(crate) use lexer_error;

macro_rules! parser_error {
    ($loc:expr, $($arg:tt)*) => {
        {
            crate::error::show_file_context($loc.clone(), format!($($arg)*));
            return Err(crate::error::Error::Parser($loc.clone(), format!($($arg)*)))
        }
    }
}
pub(crate) use parser_error;

macro_rules! eof_error {
    ($loc:expr, $($arg:tt)*) => {
        {
            crate::error::show_file_context($loc.clone(), format!($($arg)*));
            return Err(crate::error::Error::UnexpectedEOF($loc.clone(), format!("Unexpected EOF: {}", format!($($arg)*))))
        }
    }
}
pub(crate) use eof_error;

macro_rules! runtime_error {
    ($loc:expr, $($arg:tt)*) => {
        {
            crate::error::show_file_context($loc.clone(), format!($($arg)*));
            return Err(crate::error::Error::Runtime($loc.clone(), format!($($arg)*)))
        }
    }
}
pub(crate) use runtime_error;


pub fn show_file_context(loc: Location, msg: String) {
    let filename = &loc.filename;
    if filename == "<repl>" {
        return;
    }
    let file_content = std::fs::read_to_string(&filename).expect("couldn't open input file");
    let lines = file_content.lines().collect::<Vec<&str>>();
    let context = 3;
    let min_line = if loc.line <= context {
        1
    } else {
        &loc.line - context - 1
    };
    let max_line = lines.len().min(&loc.line + context);
    println!("╭─────────────────────────────────────────────────────────────────────────────────");
    println!("│ {}: Error: {}", loc.clone(), msg);
    println!("├────┬────────────────────────────────────────────────────────────────────────────");
    for line_no in min_line..max_line {
        let line = lines[line_no];
        let mut trunc = false;
        if !(line_no == loc.line - 1) {
            let line_min = 0;
            let line_max = match std::cmp::min(line.len(),75) {
                x if x == line.len() => x,
                x=> {
                    trunc = true;
                    x - 4
                }
            };
            let line = match trunc {
                true => format!("{} ...", &line[line_min..line_max]),
                false => line[line_min..line_max].to_string(),
            };
            let line_max = std::cmp::min(line.len(),75);
            println!("│{:>3} │ {}", line_no, &line[line_min..line_max]);
        } else {
            let line_max = std::cmp::min(line.len(),loc.column + 10);
            let line_min = match loc.column.saturating_sub(74) {
                0 => 0,
                x=> {
                    trunc = true;
                    x + 4
                }
            };
            let line = match trunc {
                true => format!("... {}", &line[line_min..line_max]),
                false => line[line_min..line_max].to_string(),
            };
            let column = match trunc {
                true => loc.column - line_min + 4,
                false => loc.column - line_min,
            };
            println!("│{:>3} │ {}", line_no, line);
            println!("│    ├╌{}▲", "╌".repeat((column) - 1));
            println!("|    |{}\x1b[0;31m{}\x1b[0m", " ".repeat(std::cmp::max(0, column-msg.len()+1)), msg)
        }
    }
    println!("╰────┴────────────────────────────────────────────────────────────────────────────");
}
