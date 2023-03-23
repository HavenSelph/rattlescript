use std::fmt::{Display, Error};

#[derive(Debug, Clone)]
pub struct Location {
    line: usize,
    column: usize,
    filename: String
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), Error> {
        write!(f, "{}:{}:{}", self.filename, self.line, self.column)
    }
}

#[derive(Debug, Clone)]
pub enum Token {
    IntegerLiteral(Location, i64),
    FloatLiteral(Location, f64),
    StringLiteral(Location, String),
    Plus(Location),
    Minus(Location),
    Star(Location),
    Slash(Location),
    LeftParen(Location),
    RightParen(Location),
    LeftBracket(Location),
    RightBracket(Location),
    Colon(Location),
    EOF(Location),
}

impl Token {
    pub fn location(&self) -> Location {
        match self {
            Token::IntegerLiteral(loc, _) => loc.clone(),
            Token::FloatLiteral(loc, _) => loc.clone(),
            Token::StringLiteral(loc, _) => loc.clone(),
            Token::Plus(loc) => loc.clone(),
            Token::Minus(loc) => loc.clone(),
            Token::Star(loc) => loc.clone(),
            Token::Slash(loc) => loc.clone(),
            Token::LeftParen(loc) => loc.clone(),
            Token::RightParen(loc) => loc.clone(),
            Token::LeftBracket(loc) => loc.clone(),
            Token::RightBracket(loc) => loc.clone(),
            Token::Colon(loc) => loc.clone(),
            Token::EOF(loc) => loc.clone(),
        }
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), Error> {
        match self {
            Token::IntegerLiteral(_, num) => write!(f, "IntegerLiteral({})", num),
            Token::FloatLiteral(_, num) => write!(f, "FloatLiteral({})", num),
            Token::StringLiteral(_, string) => write!(f, "StringLiteral({})", string),
            Token::Plus(_) => write!(f, "Plus"),
            Token::Minus(_) => write!(f, "Minus"),
            Token::Star(_) => write!(f, "Star"),
            Token::Slash(_) => write!(f, "Slash"),
            Token::LeftParen(_) => write!(f, "LeftParen"),
            Token::RightParen(_) => write!(f, "RightParen"),
            Token::LeftBracket(_) => write!(f, "LeftBracket"),
            Token::RightBracket(_) => write!(f, "RightBracket"),
            Token::Colon(_) => write!(f, "Colon"),
            Token::EOF(_) => write!(f, "EOF"),
        }
    }
}

#[derive(Debug)]
pub struct Lexer {
    location: Location,
    input: String,
    current_index: usize
}

impl Lexer {
    pub fn new(input: String, filename: String) -> Lexer {
        Lexer {
            location: Location {
                line: 1,
                column: 1,
                filename,
            },
            input,
            current_index: 0
        }
    }

    pub fn from_file(filename: String) -> Lexer {
        let input = std::fs::read_to_string(filename.clone()).unwrap();
        Lexer::new(input, filename)
    }

    fn cur(&self) -> Option<char> {
        self.input.chars().nth(self.current_index)
    }

    fn increment(&mut self) {
        match self.cur() {
            Some('\n') => {
                self.location.line += 1;
                self.location.column = 1;
                self.current_index += 1;
            }
            Some(_) => {
                self.current_index += 1;
                self.location.column += 1;
            }
            None => {}
        }
    }

    pub fn lex(&mut self) -> Vec<Token> {
        let mut tokens = vec![];
        while let Some(c) = self.cur() {
            match c {
                '0'..='9' => {
                    let loc = self.location.clone();
                    let mut num = String::new();
                    while let Some(c) = self.cur() {
                        match c {
                            '0'..='9' => {
                                num.push(c);
                                self.increment();
                            }
                            _ => break
                        }
                    }
                    if let Some('.') = self.cur() {
                        num.push('.');
                        self.increment();
                        while let Some(c) = self.cur() {
                            match c {
                                '0'..='9' => {
                                    num.push(c);
                                    self.increment();
                                }
                                _ => break
                            }
                        }
                        tokens.push(Token::FloatLiteral(loc, num.parse().unwrap()));
                    } else {
                        tokens.push(Token::IntegerLiteral(loc, num.parse().unwrap()));
                    }
                }
                '+' => {
                    tokens.push(Token::Plus(self.location.clone()));
                    self.increment();
                }
                '-' => {
                    tokens.push(Token::Minus(self.location.clone()));
                    self.increment();
                }
                '*' => {
                    tokens.push(Token::Star(self.location.clone()));
                    self.increment();
                }
                '/' => {
                    tokens.push(Token::Slash(self.location.clone()));
                    self.increment();
                }
                '(' => {
                    tokens.push(Token::LeftParen(self.location.clone()));
                    self.increment();
                }
                ')' => {
                    tokens.push(Token::RightParen(self.location.clone()));
                    self.increment();
                }
                '[' => {
                    tokens.push(Token::LeftBracket(self.location.clone()));
                    self.increment();
                }
                ']' => {
                    tokens.push(Token::RightBracket(self.location.clone()));
                    self.increment();
                }
                ':' => {
                    tokens.push(Token::Colon(self.location.clone()));
                    self.increment();
                }
                '"' => {
                    tokens.push(self.lex_string_literal());
                }
                ' ' => {
                    self.increment();
                }
                _ => {
                    panic!("Unexpected character: {}", c);
                }
            }
        }
        tokens.push(Token::EOF(self.location.clone()));
        return tokens;
    }

    fn lex_string_literal(&mut self) -> Token {
        let loc = self.location.clone();
        let mut string = String::new();
        self.increment();
        while let Some(c) = self.cur() {
            match c {
                '"' => {
                    self.increment();
                    break;
                }
                '\n' => {
                    panic!("{loc} Unexpected newline in string literal");
                }
                _ => {
                    string.push(c);
                    self.increment();
                }
            }
        }
        Token::StringLiteral(loc, string)
    }
}