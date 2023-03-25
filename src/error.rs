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
        return Err(crate::error::Error::Lexer($loc.clone(), format!($($arg)*)))
    }
}
pub(crate) use lexer_error;

macro_rules! parser_error {
    ($loc:expr, $($arg:tt)*) => {
        return Err(crate::error::Error::Parser($loc.clone(), format!($($arg)*)))
    }
}
pub(crate) use parser_error;

macro_rules! eof_error {
    ($loc:expr, $($arg:tt)*) => {
        return Err(crate::error::Error::UnexpectedEOF($loc.clone(), format!("Unexpected EOF: {}", format!($($arg)*))))
    }
}
pub(crate) use eof_error;

macro_rules! runtime_error {
    ($loc:expr, $($arg:tt)*) => {
        return Err(crate::error::Error::Runtime($loc.clone(), format!($($arg)*)))
    }
}
pub(crate) use runtime_error;

// TODO: refactor/remove
/*
macro_rules! _error {
    ($loc:expr, $($arg:tt)*) => {
        {
            let msg = format!($($arg)*);
            let filename = &$loc.filename;
            let file_content = std::fs::read_to_string(filename).expect("couldn't open input file");
            let lines = file_content.lines().collect::<Vec<&str>>();
            let context = 3;
            let min_line = if $loc.line <= context {
                1
            } else {
                $loc.line - context - 1
            };
            let max_line = lines.len().min($loc.line + context);

            println!("╭───────────────────────────────────────────────────────────────");
            println!("│ {}: Error: {}", $loc.clone(), msg);
            println!("├────┬──────────────────────────────────────────────────────────");

            for line_no in min_line..max_line {
                let line = lines[line_no];
                println!("│{:>3} │ {}", line_no, line);
                if line_no == $loc.line - 1 {
                    println!("│    ├─{}┘ \x1b[0;31m{}\x1b[0m", "─".repeat($loc.column - 1), msg);
                }
            }

            println!("╰────┴──────────────────────────────────────────────────────────");
            panic!();
        }
    }
}
*/
