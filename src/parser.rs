use crate::token::Token;
use crate::ast::AST;
use crate::utils::error;

pub struct Parser {
    tokens: Vec<Token>,
    current_index: usize,
}

macro_rules! consume {
    ($self:ident, $var:path) => {
        match $self.cur() {
            $var{..} => {
                let prev = $self.cur();
                $self.increment();
                prev
            },
            _ => error!("{}: Expected {}", $self.cur().location(), stringify!($var))
        }
    };

    ($self:ident, $var:path, $error:expr) => {
        match $self.cur() {
            $var{..} => {
                let prev = $self.cur();
                $self.increment();
                prev
            },
            _ => error!("{}: {}", $self.cur().location(), $error)
        }
    };
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser {
            tokens,
            current_index: 0,
        }
    }

    fn cur(&self) -> Token {
        self.tokens.get(self.current_index).cloned().expect("Unexpected end of file")
    }

    fn increment(&mut self) {
        match self.cur() {
            Token::EOF(_) => {}
            _ => { self.current_index += 1; }
        }
    }

    pub fn parse(&mut self) -> Box<AST> {
        let res = self.parse_block(/*global*/ true);
        consume!(self, Token::EOF);
        res
    }

    fn parse_block(&mut self, _global: bool) -> Box<AST> {
        let loc = self.cur().location().clone();
        let mut statements = vec![];
        loop {
            match self.cur() {
                Token::EOF(_) => break,
                _ => {}
            }
            statements.push(self.parse_statement());
        }
        Box::new(AST::Block(loc, statements))
    }

    fn parse_statement(&mut self) -> Box<AST> {
        match self.cur() {
            Token::Let(loc) => {
                self.increment();
                if let Token::Identifier(_, name) = self.cur() {
                    self.increment();
                    consume!(self, Token::Equals);
                    let expr = self.parse_expression();
                    consume!(self, Token::SemiColon);
                    Box::new(AST::VarDeclaration(loc.clone(), name, expr))

                } else {
                    error!("{}: Expected identifier after `let`", self.cur().location());
                }
            }
            _ => {
                let expr = self.parse_expression();
                consume!(self, Token::SemiColon, format!("Expected `;` after lone expression"));
                expr
            }
        }
    }

    fn parse_expression(&mut self) -> Box<AST> {
        self.parse_additive()
    }

    fn parse_additive(&mut self) -> Box<AST> {
        let mut left = self.parse_multiplicative();
        loop {
            match self.cur() {
                Token::Plus(_) | Token::Minus(_) => {
                    let op = self.cur().clone();
                    self.increment();
                    let right = self.parse_multiplicative();
                    left = match op {
                        Token::Plus(loc) => Box::new(AST::Plus(loc, left, right)),
                        Token::Minus(loc) => Box::new(AST::Minus(loc, left, right)),
                        _ => unreachable!()
                    }
                },
                _ => return left
            }
        }
    }

    fn parse_multiplicative(&mut self) -> Box<AST> {
        let mut left = self.parse_postfix();
        loop {
            match self.cur() {
                Token::Star(_) | Token::Slash(_) => {
                    let op = self.cur().clone();
                    self.increment();
                    let right = self.parse_postfix();
                    left = match op {
                        Token::Star(loc) => Box::new(AST::Multiply(loc, left, right)),
                        Token::Slash(loc) => Box::new(AST::Divide(loc, left, right)),
                        _ => unreachable!()
                    }
                },
                _ => return left
            }
        }
    }

    fn parse_slice_value(&mut self) -> Option<Box<AST>> {
        match self.cur() {
            Token::Colon(_) | Token::RightBracket(_) => {
                None
            }
            _ => Some(self.parse_expression())
        }
    }

    fn parse_postfix(&mut self) -> Box<AST> {
        let mut val = self.parse_atom();
        loop {
            match self.cur() {
                Token::LeftBracket(loc) => {
                    self.increment();

                    let start = self.parse_slice_value();
                    if let Token::RightBracket(_) = self.cur() {
                        println!("{}: Empty index", loc);
                        if let Some(start) = start {
                            self.increment();
                            val = Box::new(AST::Index(loc.clone(), val, start));
                            continue;

                        } else {
                            error!("{}: Cannot have empty index", loc);
                        }
                    }

                    consume!(self, Token::Colon);
                    let end = self.parse_slice_value();

                    if let Token::RightBracket(_) = self.cur() {
                        self.increment();
                        val = Box::new(AST::Slice{loc:loc.clone(), lhs:val, start, end, step: None});
                        continue;
                    }

                    consume!(self, Token::Colon);
                    let step = self.parse_slice_value();
                    consume!(self, Token::RightBracket);
                    val = Box::new(AST::Slice {loc, lhs: val, start, end, step})
                },
                Token::LeftParen(loc) => {
                    self.increment();
                    let mut args = vec![];
                    loop {
                        match self.cur() {
                            Token::RightParen(_) => {
                                self.increment();
                                break;
                            }
                            _ => {
                                args.push(self.parse_expression());
                                match self.cur() {
                                    Token::Comma(_) => {
                                        self.increment();
                                    }
                                    Token::RightParen(_) => {}
                                    _ => error!("{}: Expected `)` or `,`", self.cur().location())
                                }
                            }
                        }
                    }
                    val = Box::new(AST::Call(loc, val, args));
                }
                _ => break,
            }
        }
        val
    }

    fn parse_atom(&mut self) -> Box<AST> {
        match self.cur() {
            Token::LeftParen(_) => {
                self.increment();
                let expr = self.parse_expression();
                match self.cur() {
                    Token::RightParen(_) => {
                        self.increment();
                        expr
                    },
                    _ => error!("{}: Expected ')'", self.cur().location())
                }
            }
            Token::IntegerLiteral(loc, num) => {
                self.increment();
                Box::new(AST::IntegerLiteral(loc, num))
            },
            Token::FloatLiteral(loc, num) => {
                self.increment();
                Box::new(AST::FloatLiteral(loc, num))
            },
            Token::StringLiteral(loc, string) => {
                self.increment();
                Box::new(AST::StringLiteral(loc, string))
            },
            Token::Identifier(loc, name) => {
                self.increment();
                Box::new(AST::Variable(loc, name))
            },
            _ => error!("{}: Unexpected token in parse_atom: {}", self.cur().location(), self.cur())
        }
    }
}