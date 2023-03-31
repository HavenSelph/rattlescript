use crate::common::Span;

#[derive(Debug)]
pub enum ErrorKind {
    Lexer,
    Parser,
    UnexpectedEOF,
    Runtime,
}

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
    pub message: String,
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ErrorKind::Lexer | ErrorKind::Parser | ErrorKind::UnexpectedEOF => {
                write!(f, "SyntaxError: {}", self.message)
            }
            ErrorKind::Runtime => write!(f, "RuntimeError: {}", self.message),
        }
    }
}

pub type Result<T> = std::result::Result<T, Error>;

macro_rules! lexer_error {
    ($span:expr, $($arg:tt)*) => {
        return Err(crate::error::Error{
            kind: crate::error::ErrorKind::Lexer,
            span: $span.clone(),
            message: format!($($arg)*),
        })
    }
}
pub(crate) use lexer_error;

macro_rules! parser_error {
    ($span:expr, $($arg:tt)*) => {
        return Err(crate::error::Error{
            kind: crate::error::ErrorKind::Parser,
            span: $span.clone(),
            message: format!($($arg)*),
        })
    }
}
pub(crate) use parser_error;

macro_rules! eof_error {
    ($span:expr, $($arg:tt)*) => {
        return Err(crate::error::Error{
            kind: crate::error::ErrorKind::UnexpectedEOF,
            span: $span.clone(),
            message: format!("Unexpected EOF: {}", format!($($arg)*)),
        })
    }
}
pub(crate) use eof_error;

macro_rules! runtime_error {
    ($span:expr, $($arg:tt)*) => {
        return Err(crate::error::Error{
            kind: crate::error::ErrorKind::Runtime,
            span: $span.clone(),
            message: format!($($arg)*),
        })
    }
}
pub(crate) use runtime_error;


impl Error {
    pub fn print_with_source(&self) {
        let msg = &self.message;
        let filename = &self.span.0.filename;
        let file_content = match std::fs::read_to_string(filename) {
            Ok(content) => content,
            Err(_) => {
                println!("{}: Error: {}", self.span.0, msg);
                return;
            }
        };
        let lines = file_content.lines().collect::<Vec<&str>>();
        let context = 3;

        let start = self.span.0;
        let end = self.span.1;

        let min_line = if start.line <= context {
            1
        } else {
            start.line - context - 1
        };
        let max_line = lines.len().min(end.line + context);

        println!("╭────────────────────────────────────────────────────────────────────────────────");
        println!("│ {}: Error: {}", start, msg);
        println!("├─────┬──────────────────────────────────────────────────────────────────────────");
        #[allow(clippy::needless_range_loop)]
        for line_no in min_line..max_line {
            let line = lines[line_no];
            if start.line - 1 <= line_no && line_no < end.line {
                let highlight_start = if line_no == start.line - 1 {
                    start.column - 1
                } else {
                    0
                };
                let highlight_end = if line_no == end.line - 1 {
                    end.column - 1
                } else {
                    line.len()
                };

                let text_before = &line[..highlight_start];
                let text_highlight = &line[highlight_start..highlight_end];
                let text_after = &line[highlight_end..];
                println!("│ {:>3} │ {}\x1b[0;31m{}\x1b[0m{}", line_no, text_before, text_highlight, text_after);

                if start.line == end.line {
                    if text_highlight.len() <= 1 {
                        println!("│     │ {}\x1b[0;31m▲\x1b[0m", " ".repeat(text_before.len()));
                    } else {
                        println!("│     │ {}\x1b[0;31m└{}┘\x1b[0m", " ".repeat(text_before.len()), "─".repeat(text_highlight.len()-2));
                    }
                }
            } else {
                println!("│ {:>3} │ {}", line_no, line);
            }
        }

        println!("╰─────┴──────────────────────────────────────────────────────────────────────────");
    }
}
