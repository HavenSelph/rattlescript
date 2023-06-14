use crate::common::Span;

#[allow(clippy::upper_case_acronyms)]
#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenKind {
    And,
    As,
    Assert,
    At,
    Bang,
    BangEquals,
    Break,
    Class,
    Colon,
    Comma,
    Continue,
    Def,
    Dot,
    DotDot,
    EOF,
    Elif,
    Else,
    Equals,
    EqualsEquals,
    False,
    FatArrow,
    FloatLiteral,
    For,
    FormatStringLiteral,
    From,
    GreaterEquals,
    GreaterThan,
    Identifier,
    If,
    Import,
    In,
    IntegerLiteralBin,
    IntegerLiteralDec,
    IntegerLiteralHex,
    IntegerLiteralOct,
    LeftBrace,
    LeftBracket,
    LeftParen,
    LessEquals,
    LessThan,
    Let,
    Minus,
    MinusEquals,
    MinusMinus,
    Not,
    Namespace,
    Nothing,
    Or,
    Percent,
    Pipe,
    Plus,
    PlusEquals,
    PlusPlus,
    Return,
    RightBrace,
    RightBracket,
    RightParen,
    SemiColon,
    Slash,
    SlashEquals,
    Star,
    StarEquals,
    StarStar,
    Static,
    StringLiteral,
    True,
    While,
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
                "as" => TokenKind::As,
                "assert" => TokenKind::Assert,
                "break" => TokenKind::Break,
                "class" => TokenKind::Class,
                "continue" => TokenKind::Continue,
                "def" => TokenKind::Def,
                "elif" => TokenKind::Elif,
                "else" => TokenKind::Else,
                "false" => TokenKind::False,
                "for" => TokenKind::For,
                "from" => TokenKind::From,
                "if" => TokenKind::If,
                "import" => TokenKind::Import,
                "in" => TokenKind::In,
                "let" => TokenKind::Let,
                "not" => TokenKind::Not,
                "namespace" => TokenKind::Namespace,
                "nothing" => TokenKind::Nothing,
                "or" => TokenKind::Or,
                "return" => TokenKind::Return,
                "static" => TokenKind::Static,
                "true" => TokenKind::True,
                "while" => TokenKind::While,
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
