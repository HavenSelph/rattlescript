use crate::token::{Location, Token, TokenKind};
use crate::utils::{lexer_error as error, Result};

#[derive(Debug)]
pub struct Lexer {
    location: Location,
    input: String,
    current_index: usize,
    seen_newline: bool,
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
            current_index: 0,
            seen_newline: false,
        }
    }

    fn cur(&self) -> Option<char> {
        self.input.chars().nth(self.current_index)
    }

    fn peek(&self, offset: usize) -> Option<char> {
        self.input.chars().nth(self.current_index + offset)
    }

    fn increment(&mut self) {
        match self.cur() {
            Some('\n') => {
                self.location.line += 1;
                self.location.column = 1;
                self.current_index += 1;
                self.seen_newline = true;
            }
            Some(_) => {
                self.current_index += 1;
                self.location.column += 1;
            }
            None => {}
        }
    }

    fn push_simple(&mut self, tokens: &mut Vec<Token>, kind: TokenKind, len: usize) {
        self.push(
            tokens,
            Token::new(
                kind,
                self.location.clone(),
                self.input[self.current_index..self.current_index + len].to_string(),
            ),
        );
        for _ in 0..len {
            self.increment();
        }
    }

    fn push(&mut self, tokens: &mut Vec<Token>, mut token: Token) {
        token.newline_before = self.seen_newline;
        tokens.push(token);
        self.seen_newline = false;
    }

    pub fn lex(&mut self) -> Result<Vec<Token>> {
        let mut tokens: Vec<Token> = vec![];
        while let Some(c) = self.cur() {
            match c {
                c if c.is_whitespace() => self.increment(),
                '0'..='9' => {
                    let loc = self.location.clone();
                    let mut num = String::new();
                    while let Some(c) = self.cur() {
                        match c {
                            '0'..='9' => {
                                num.push(c);
                                self.increment();
                            }
                            _ => break,
                        }
                    }
                    if let Some('.') = self.cur() {
                        if let Some('.') = self.peek(1) {
                            self.push(
                                &mut tokens,
                                Token::new(TokenKind::IntegerLiteral, loc.clone(), num),
                            );
                        } else {
                            num.push('.');
                            self.increment();
                            while let Some(c) = self.cur() {
                                match c {
                                    '0'..='9' => {
                                        num.push(c);
                                        self.increment();
                                    }
                                    _ => break,
                                }
                            }
                            self.push(&mut tokens, Token::new(TokenKind::FloatLiteral, loc, num));
                        }
                    } else {
                        self.push(&mut tokens, Token::new(TokenKind::IntegerLiteral, loc, num));
                    }
                }
                '+' => self.push_simple(&mut tokens, TokenKind::Plus, 1),
                '-' => self.push_simple(&mut tokens, TokenKind::Minus, 1),
                '*' => self.push_simple(&mut tokens, TokenKind::Star, 1),
                '/' => match self.peek(1) {
                    Some('/') => {
                        while let Some(c) = self.cur() {
                            self.increment();
                            if c == '\n' {
                                break;
                            }
                        }
                    }
                    _ => self.push_simple(&mut tokens, TokenKind::Slash, 1),
                },
                '(' => self.push_simple(&mut tokens, TokenKind::LeftParen, 1),
                ')' => self.push_simple(&mut tokens, TokenKind::RightParen, 1),
                '[' => self.push_simple(&mut tokens, TokenKind::LeftBracket, 1),
                ']' => self.push_simple(&mut tokens, TokenKind::RightBracket, 1),
                '|' => self.push_simple(&mut tokens, TokenKind::Pipe, 1),
                ':' => self.push_simple(&mut tokens, TokenKind::Colon, 1),
                '=' => match self.peek(1) {
                    Some('>') => self.push_simple(&mut tokens, TokenKind::FatArrow, 2),
                    Some('=') => self.push_simple(&mut tokens, TokenKind::EqualsEquals, 2),
                    _ => self.push_simple(&mut tokens, TokenKind::Equals, 1),
                },
                '<' => match self.peek(1) {
                    Some('=') => self.push_simple(&mut tokens, TokenKind::LessThanEquals, 2),
                    _ => self.push_simple(&mut tokens, TokenKind::LessThan, 1),
                },
                '>' => match self.peek(1) {
                    Some('=') => self.push_simple(&mut tokens, TokenKind::GreaterThanEquals, 2),
                    _ => self.push_simple(&mut tokens, TokenKind::GreaterThan, 1),
                },
                '!' => match self.peek(1) {
                    Some('=') => self.push_simple(&mut tokens, TokenKind::BangEquals, 2),
                    _ => self.push_simple(&mut tokens, TokenKind::Bang, 1),
                },
                ';' => self.push_simple(&mut tokens, TokenKind::SemiColon, 1),
                ',' => self.push_simple(&mut tokens, TokenKind::Comma, 1),
                '{' => self.push_simple(&mut tokens, TokenKind::LeftBrace, 1),
                '}' => self.push_simple(&mut tokens, TokenKind::RightBrace, 1),
                '@' => self.push_simple(&mut tokens, TokenKind::At, 1),
                '"' => {
                    let token = self.lex_string_literal()?;
                    self.push(&mut tokens, token);
                }
                '.' => match self.peek(1) {
                    Some('.') => self.push_simple(&mut tokens, TokenKind::DotDot, 2),
                    _ => self.push_simple(&mut tokens, TokenKind::Dot, 1),
                },
                'a'..='z' | 'A'..='Z' | '_' => {
                    let loc = self.location.clone();
                    let mut ident = String::new();
                    while let Some(c) = self.cur() {
                        match c {
                            'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => {
                                ident.push(c);
                                self.increment();
                            }
                            _ => break,
                        }
                    }
                    self.push(&mut tokens, Token::from_str(ident, loc));
                }
                _ => error!(self.location, "Unexpected character {}", c),
            }
        }
        self.push_simple(&mut tokens, TokenKind::Eof, 0);
        Ok(tokens)
    }

    fn lex_string_literal(&mut self) -> Result<Token> {
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
        Ok(Token::new(TokenKind::StringLiteral, loc, string))
    }
}
