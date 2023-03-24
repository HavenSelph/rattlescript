use crate::token::{Location, Token};

#[derive(Debug)]
pub struct Lexer {
    pub location: Location,
    pub input: String,
    pub current_index: usize
}

macro_rules! push_single {
    ($self:ident, $tokens:ident, $type:path) => {
        {
            $tokens.push($type($self.location.clone()));
            $self.increment();
        }
    }
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
                ' ' | '\t' | '\r' | '\n' => self.increment(),
                '+' => push_single!(self, tokens, Token::Plus),
                '-' => push_single!(self, tokens, Token::Minus),
                '*' => push_single!(self, tokens, Token::Star),
                '/' => push_single!(self, tokens, Token::Slash),
                '(' => push_single!(self, tokens, Token::LeftParen),
                ')' => push_single!(self, tokens, Token::RightParen),
                '[' => push_single!(self, tokens, Token::LeftBracket),
                ']' => push_single!(self, tokens, Token::RightBracket),
                ':' => push_single!(self, tokens, Token::Colon),
                '=' => push_single!(self, tokens, Token::Equals),
                ';' => push_single!(self, tokens, Token::SemiColon),
                ',' => push_single!(self, tokens, Token::Comma),
                '"' => tokens.push(self.lex_string_literal()),
                'a'..='z' | 'A'..='Z' | '_' => {
                    let loc = self.location.clone();
                    let mut ident = String::new();
                    while let Some(c) = self.cur() {
                        match c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                                ident.push(c);
                                self.increment();
                            }
                            _ => break
                        }
                    }
                    tokens.push(Token::from_str(ident, loc));
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