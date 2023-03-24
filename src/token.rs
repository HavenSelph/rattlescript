use std::fmt::{Display, Error};

#[derive(Debug, Clone)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub filename: String
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}:{}:{}", self.filename, self.line, self.column)
    }
}


#[derive(Debug, Clone)]
pub enum Token {
    Colon(Location),
    Comma(Location),
    Equals(Location),
    EOF(Location),
    FloatLiteral(Location, f64),
    Identifier(Location, String),
    IntegerLiteral(Location, i64),
    LeftBracket(Location),
    LeftParen(Location),
    Let(Location),
    Minus(Location),
    Plus(Location),
    RightBracket(Location),
    RightParen(Location),
    SemiColon(Location),
    Slash(Location),
    Star(Location),
    StringLiteral(Location, String),
}

impl Token {
    pub fn location(&self) -> Location {
        match self {
            Token::Colon(loc) => loc.clone(),
            Token::Comma(loc) => loc.clone(),
            Token::Equals(loc) => loc.clone(),
            Token::EOF(loc) => loc.clone(),
            Token::FloatLiteral(loc, _) => loc.clone(),
            Token::Identifier(loc, _) => loc.clone(),
            Token::IntegerLiteral(loc, _) => loc.clone(),
            Token::LeftBracket(loc) => loc.clone(),
            Token::LeftParen(loc) => loc.clone(),
            Token::Let(loc) => loc.clone(),
            Token::Minus(loc) => loc.clone(),
            Token::Plus(loc) => loc.clone(),
            Token::RightBracket(loc) => loc.clone(),
            Token::RightParen(loc) => loc.clone(),
            Token::SemiColon(loc) => loc.clone(),
            Token::Slash(loc) => loc.clone(),
            Token::Star(loc) => loc.clone(),
            Token::StringLiteral(loc, _) => loc.clone(),
        }
    }

    pub fn from_str(s: String, loc: Location) -> Token {
        match s.as_ref() {
            "let" => Token::Let(loc),
            _ => Token::Identifier(loc, s)
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), Error> {
        match self {
            Token::Colon(_) => write!(f, "Colon"),
            Token::Comma(_) => write!(f, "Comma"),
            Token::Equals(_) => write!(f, "Equals"),
            Token::EOF(_) => write!(f, "EOF"),
            Token::FloatLiteral(_, num) => write!(f, "FloatLiteral({})", num),
            Token::Identifier(_, string) => write!(f, "Identifier({})", string),
            Token::IntegerLiteral(_, num) => write!(f, "IntegerLiteral({})", num),
            Token::LeftBracket(_) => write!(f, "LeftBracket"),
            Token::LeftParen(_) => write!(f, "LeftParen"),
            Token::Let(_) => write!(f, "Let"),
            Token::Minus(_) => write!(f, "Minus"),
            Token::Plus(_) => write!(f, "Plus"),
            Token::RightBracket(_) => write!(f, "RightBracket"),
            Token::RightParen(_) => write!(f, "RightParen"),
            Token::SemiColon(_) => write!(f, "SemiColon"),
            Token::Slash(_) => write!(f, "Slash"),
            Token::Star(_) => write!(f, "Star"),
            Token::StringLiteral(_, string) => write!(f, "StringLiteral({})", string),
        }
    }
}
