use std::fmt::{Display, Error};

#[derive(Debug, Clone)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub filename: String,
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}:{}:{}", self.filename, self.line, self.column)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    And,
    Assert,
    At,
    Bang,
    BangEquals,
    Colon,
    Comma,
    Def,
    Dot,
    DotDot,
    Eof,
    Else,
    Equals,
    EqualsEquals,
    False,
    FatArrow,
    FloatLiteral,
    For,
    Identifier,
    If,
    In,
    IntegerLiteral,
    LeftBrace,
    LeftBracket,
    LeftParen,
    LessThan,
    LessThanEquals,
    Let,
    Minus,
    Not,
    GreaterThan,
    GreaterThanEquals,
    Nothing,
    Or,
    Pipe,
    Plus,
    Return,
    RightBrace,
    RightBracket,
    RightParen,
    SemiColon,
    Slash,
    Star,
    StringLiteral,
    True,
    While,
    Continue,
    Break,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Location,
    pub text: String,
    pub newline_before: bool,
}

impl Token {
    pub fn new(kind: TokenKind, loc: Location, text: String) -> Token {
        Token {
            kind,
            loc,
            text,
            newline_before: false,
        }
    }

    pub fn from_str(text: String, loc: Location) -> Token {
        Token {
            kind: match text.as_ref() {
                "and" => TokenKind::And,
                "assert" => TokenKind::Assert,
                "def" => TokenKind::Def,
                "else" => TokenKind::Else,
                "false" => TokenKind::False,
                "if" => TokenKind::If,
                "let" => TokenKind::Let,
                "not" => TokenKind::Not,
                "nothing" => TokenKind::Nothing,
                "or" => TokenKind::Or,
                "return" => TokenKind::Return,
                "true" => TokenKind::True,
                "while" => TokenKind::While,
                "continue" => TokenKind::Continue,
                "break" => TokenKind::Break,
                "for" => TokenKind::For,
                "in" => TokenKind::In,
                _ => TokenKind::Identifier,
            },
            loc,
            text,
            newline_before: false,
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), Error> {
        match self.kind {
            TokenKind::IntegerLiteral => write!(f, "IntegerLiteral({})", self.text),
            TokenKind::FloatLiteral => write!(f, "FloatLiteral({})", self.text),
            TokenKind::StringLiteral => write!(f, "StringLiteral({})", self.text),
            TokenKind::Identifier => write!(f, "Identifier({})", self.text),
            _ => write!(f, "{:?}", self.kind),
        }
    }
}
