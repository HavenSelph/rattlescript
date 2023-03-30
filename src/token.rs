
use crate::common::Span;

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, Eq, PartialEq)]
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
    EOF,
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
    IntegerLiteralBin,
    IntegerLiteralOct,
    IntegerLiteralDec,
    IntegerLiteralHex,
    LeftBrace,
    LeftBracket,
    LeftParen,
    LessThan,
    LessEquals,
    Let,
    Minus,
    MinusMinus,
    Not,
    GreaterThan,
    GreaterEquals,
    Nothing,
    Or,
    Pipe,
    Plus,
    PlusPlus,
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
    pub span: Span,
    pub text: String,
    pub newline_before: bool,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span, text: String) -> Token {
        Token {
            kind,
            span,
            text,
            newline_before: false,
        }
    }

    pub fn from_str(text: String, span: Span) -> Token {
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
            span,
            text,
            newline_before: false,
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{:?}", self.kind)?;
        if !self.text.is_empty() {
            write!(f, "({})", self.text)?;
        }
        Ok(())
    }
}
