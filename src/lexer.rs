use crate::error::{lexer_error as error, Result};
use crate::common::{Location, Span};
use crate::token::{Token, TokenKind};

#[derive(Debug)]
pub struct Lexer {
    pub location: Location,
    input: String,
    current_index: usize,
    seen_newline: bool,
}

impl Lexer {
    pub fn new(input: String, filename: &'static str) -> Lexer {
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

    fn loc(&self) -> Location {
        self.location
    }

    fn push_simple(&mut self, tokens: &mut Vec<Token>, kind: TokenKind, len: usize) {
        let start = self.loc();
        let text = self.input[self.current_index..self.current_index + len].to_string();
        for _ in 0..len {
            self.increment();
        }
        self.push(tokens, Token::new(kind, Span(start, self.loc()), text));
    }

    fn push(&mut self, tokens: &mut Vec<Token>, mut token: Token) {
        token.newline_before = self.seen_newline;
        tokens.push(token);
        self.seen_newline = false;
    }

    pub fn lex(&mut self) -> Result<Vec<Token>> {
        let mut tokens: Vec<Token> = vec![];
        while let Some(c) = self.cur() {
            let start = self.loc();
            match c {
                c if c.is_whitespace() => self.increment(),

                // base N literals, i.e. 0b_1101, 0o_567, 0x_ff
                '0' if self.peek(1).map_or(false, |c| "box".contains(c)) => {
                    let mut num = String::new();

                    let base = match self.peek(1) {
                        Some('b') => Base::Bin,
                        Some('o') => Base::Oct,
                        Some('x') => Base::Hex,
                        _ => Base::Dec,
                    };

                    self.increment();
                    self.increment();

                    self.lex_num(&mut num, base, &start)?;
                    self.push(
                        &mut tokens,
                        Token::new(base.into(), Span(start, self.loc()), num),
                    );
                }

                // decimal int/float literals
                '0'..='9' => {
                    let mut num = String::new();

                    self.lex_num(&mut num, Base::Dec, &start)?;
                    if let Some('.') = self.cur() {
                        if let Some('.') = self.peek(1) {
                            self.push(
                                &mut tokens,
                                Token::new(
                                    TokenKind::IntegerLiteralDec,
                                    Span(start, self.loc()),
                                    num,
                                ),
                            );
                        } else {
                            num.push('.');
                            self.increment();
                            self.lex_num(&mut num, Base::Dec, &start)?;
                            self.push(
                                &mut tokens,
                                Token::new(TokenKind::FloatLiteral, Span(start, self.loc()), num),
                            );
                        }
                    } else {
                        self.push(
                            &mut tokens,
                            Token::new(TokenKind::IntegerLiteralDec, Span(start, self.loc()), num),
                        );
                    }
                }
                '+' => match self.peek(1) {
                    Some('+') => self.push_simple(&mut tokens, TokenKind::PlusPlus, 2),
                    _ => self.push_simple(&mut tokens, TokenKind::Plus, 1),
                },
                '-' => match self.peek(1) {
                    Some('-') => self.push_simple(&mut tokens, TokenKind::MinusMinus, 2),
                    _ => self.push_simple(&mut tokens, TokenKind::Minus, 1),
                },
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
                    Some('=') => self.push_simple(&mut tokens, TokenKind::LessEquals, 2),
                    _ => self.push_simple(&mut tokens, TokenKind::LessThan, 1),
                },
                '>' => match self.peek(1) {
                    Some('=') => self.push_simple(&mut tokens, TokenKind::GreaterEquals, 2),
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
                '"' | '`' => {
                    let token = self.lex_string_literal()?;
                    self.push(&mut tokens, token);
                }
                '.' => match self.peek(1) {
                    Some('.') => self.push_simple(&mut tokens, TokenKind::DotDot, 2),
                    _ => self.push_simple(&mut tokens, TokenKind::Dot, 1),
                },

                // identifiers
                'a'..='z' | 'A'..='Z' | '_' => {
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
                    self.push(&mut tokens, Token::from_str(ident, Span(start, self.loc())));
                }
                _ => error!(Span(start, self.loc()), "Unexpected character {}", c),
            }
        }
        self.push_simple(&mut tokens, TokenKind::EOF, 0);
        Ok(tokens)
    }

    fn lex_string_literal(&mut self) -> Result<Token> {
        let start = self.loc();
        let mut string = String::new();
        let quote = self.cur().unwrap();
        self.increment();
        while let Some(c) = self.cur() {
            match c {
                c if c==quote => {
                    self.increment();
                    return Ok(Token::new(
                        if quote == '"' {
                            TokenKind::StringLiteral
                        } else {
                            TokenKind::FormatStringLiteral
                        },
                        Span(start, self.loc()),
                        string,
                    ));
                }
                '\n' => break,
                _ => {
                    string.push(c);
                    self.increment();
                }
            }
        }
        error!(Span(start, self.loc()), "Unterminated string literal");
    }

    fn lex_num(&mut self, num: &mut String, base: Base, start: &Location) -> Result<()> {
        while let Some(mut c) = self.cur() {
            c = c.to_ascii_lowercase();
            match (base, c) {
                (Base::Bin, '0'..='1')
                | (Base::Oct, '0'..='7')
                | (Base::Dec, '0'..='9')
                | (Base::Hex, '0'..='9' | 'a'..='f') => {
                    num.push(c);
                    self.increment();
                }
                (_, '0'..='9' | 'a'..='f') => {
                    error!(Span(*start, self.loc()), "Invalid numerical literal");
                }
                (_, '_') => self.increment(),
                _ => break,
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy)]
enum Base {
    Bin,
    Oct,
    Dec,
    Hex,
}

impl From<Base> for TokenKind {
    fn from(value: Base) -> Self {
        match value {
            Base::Bin => Self::IntegerLiteralBin,
            Base::Oct => Self::IntegerLiteralOct,
            Base::Dec => Self::IntegerLiteralDec,
            Base::Hex => Self::IntegerLiteralHex,
        }
    }
}
