use std::sync::Arc;

use crate::token::{Token, TokenKind};
use crate::ast::AST;
use crate::utils::error;

pub struct Parser {
    tokens: Vec<Token>,
    current_index: usize,
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
        match self.cur().kind {
            TokenKind::EOF => {}
            _ => { self.current_index += 1; }
        }
    }

    fn consume(&mut self, kind: TokenKind) -> Token {
        if self.cur().kind == kind {
            let res = self.cur();
            self.increment();
            res.clone()
        } else {
            error!("{}: Expected token {:?}, but got {:?}", self.cur().loc, kind, self.cur().kind);
        }
    }

    pub fn parse(&mut self) -> Arc<AST> {
        let res = self.parse_block(/*global*/ true);
        self.consume(TokenKind::EOF);
        res
    }

    fn parse_block(&mut self, global: bool) -> Arc<AST> {
        let loc = self.cur().loc.clone();
        let mut statements = vec![];
        if !global {
            self.consume(TokenKind::LeftBrace);
        }
        loop {
            if !global && self.cur().kind == TokenKind::RightBrace {
                self.increment();
                break;
            }
            if global && self.cur().kind == TokenKind::EOF {
                break;
            }
            statements.push(self.parse_statement());
        }
        Arc::new(AST::Block(loc, statements))
    }

    fn consume_line_end(&mut self) {
        if self.cur().newline_before {
            return;
        }
        match self.cur().kind {
            TokenKind::SemiColon => self.increment(),
            TokenKind::EOF => {}
            _ => error!("{}: Expected line end, but got {:?}", self.cur().loc, self.cur().kind),
        }
    }

    fn parse_function(&mut self) -> (Arc<AST>, String) {
        let loc = self.consume(TokenKind::Def).loc.clone();
        let name = self.consume(TokenKind::Identifier);
        self.consume(TokenKind::LeftParen);
        let mut args = vec![];
        while self.cur().kind != TokenKind::RightParen {
            args.push(self.consume(TokenKind::Identifier).text);
            if self.cur().kind == TokenKind::Comma {
                self.increment();
            }
        }
        self.increment();
        let body = self.parse_block(/*global*/ false);
        self.consume_line_end();
        (Arc::new(AST::Function{loc, name: name.text.clone(), args, body}), name.text)
    }

    fn parse_statement(&mut self) -> Arc<AST> {
        match self.cur() {
            Token { kind: TokenKind::Let, loc, .. } => {
                self.increment();
                let ident = self.consume(TokenKind::Identifier);
                self.consume(TokenKind::Equals);
                let expr = self.parse_expression();
                self.consume_line_end();
                Arc::new(AST::VarDeclaration(loc, ident.text, expr))
            }
            Token { kind: TokenKind::Def, ..} => {
                self.parse_function().0
            },
            Token { kind: TokenKind::At, loc, ..} => {
                self.increment();
                let deco = self.parse_postfix();
                self.consume_line_end();
                let (func, name) = self.parse_function();
                self.consume_line_end();
                Arc::new(AST::Assignment(
                    loc.clone(),
                    Arc::new(AST::Variable(loc.clone(), name)),
                    Arc::new(AST::Call(
                        loc.clone(),
                        deco,
                        vec![func]
                    ))
                ))
            }
            Token { kind: TokenKind::Return, loc, ..} => {
                self.increment();
                let expr = self.parse_expression();
                self.consume_line_end();
                Arc::new(AST::Return(loc, expr))
            }
            _ => {
                let expr = self.parse_expression();
                self.consume_line_end();
                expr
            }
        }
    }

    fn parse_expression(&mut self) -> Arc<AST> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Arc<AST> {
        let left = self.parse_additive();
        match self.cur() {
            Token { kind: TokenKind::Equals, loc, ..} => {
                self.increment();
                let right = self.parse_additive();
                Arc::new(AST::Assignment(loc, left, right))
            }
            _ => left
        }
    }

    fn parse_additive(&mut self) -> Arc<AST> {
        let mut left = self.parse_multiplicative();
        loop {
            match self.cur() {
                Token { kind: TokenKind::Plus | TokenKind::Minus, loc, ..} => {
                    let op = self.cur().kind;
                    self.increment();
                    let right = self.parse_multiplicative();
                    left = match op {
                        TokenKind::Plus => Arc::new(AST::Plus(loc, left, right)),
                        TokenKind::Minus => Arc::new(AST::Minus(loc, left, right)),
                        _ => unreachable!()
                    }
                },
                _ => return left
            }
        }
    }

    fn parse_multiplicative(&mut self) -> Arc<AST> {
        let mut left = self.parse_postfix();
        loop {
            match self.cur() {
                Token { kind: TokenKind::Star | TokenKind::Slash, loc, ..} => {
                    let op = self.cur().kind;
                    self.increment();
                    let right = self.parse_postfix();
                    left = match op {
                        TokenKind::Star => Arc::new(AST::Multiply(loc, left, right)),
                        TokenKind::Slash => Arc::new(AST::Divide(loc, left, right)),
                        _ => unreachable!()
                    }
                },
                _ => return left
            }
        }
    }

    fn parse_slice_value(&mut self) -> Option<Arc<AST>> {
        match self.cur().kind {
            TokenKind::Colon | TokenKind::RightBracket => None,
            _ => Some(self.parse_expression())
        }
    }

    fn parse_postfix(&mut self) -> Arc<AST> {
        let mut val = self.parse_atom();
        loop {
            match self.cur() {
                Token { kind: TokenKind::LeftBracket, loc, .. } => {
                    self.increment();

                    let start = self.parse_slice_value();
                    if self.cur().kind == TokenKind::RightBracket {
                        if let Some(start) = start {
                            self.increment();
                            val = Arc::new(AST::Index(loc.clone(), val, start));
                            continue;

                        } else {
                            error!("{}: Cannot have empty index", loc);
                        }
                    }

                    self.consume(TokenKind::Colon);
                    let end = self.parse_slice_value();

                    if self.cur().kind == TokenKind::RightBracket {
                        self.increment();
                        val = Arc::new(AST::Slice{loc:loc.clone(), lhs:val, start, end, step: None});
                        continue;
                    }

                    self.consume(TokenKind::Colon);
                    let step = self.parse_slice_value();
                    self.consume(TokenKind::RightBracket);
                    val = Arc::new(AST::Slice {loc, lhs: val, start, end, step})
                },
                Token { kind: TokenKind::LeftParen, loc, .. } => {
                    self.increment();
                    let mut args = vec![];
                    loop {
                        match self.cur().kind {
                            TokenKind::RightParen => {
                                self.increment();
                                break;
                            }
                            _ => {
                                args.push(self.parse_expression());
                                match self.cur().kind {
                                    TokenKind::Comma => self.increment(),
                                    TokenKind::RightParen => {}
                                    _ => error!("{}: Expected `)` or `,`", self.cur().loc)
                                }
                            }
                        }
                    }
                    val = Arc::new(AST::Call(loc, val, args));
                }
                _ => break,
            }
        }
        val
    }

    fn parse_atom(&mut self) -> Arc<AST> {
        match self.cur() {
            Token { kind: TokenKind::LeftParen, .. } => {
                self.increment();
                let expr = self.parse_expression();
                match self.cur().kind {
                    TokenKind::RightParen => {
                        self.increment();
                        expr
                    },
                    _ => error!("{}: Expected ')'", self.cur().loc)
                }
            }
            Token { kind: TokenKind::IntegerLiteral, loc, text, ..} => {
                self.increment();
                if let Some(num) = text.parse::<i64>().ok() {
                    Arc::new(AST::IntegerLiteral(loc, num))
                } else {
                    error!("{}: Invalid integer literal: {}", loc, text);
                }
            },
            Token { kind: TokenKind::FloatLiteral, loc, text, ..} => {
                self.increment();
                if let Some(num) = text.parse::<f64>().ok() {
                    Arc::new(AST::FloatLiteral(loc, num))
                } else {
                    error!("{}: Invalid float literal: {}", loc, text);
                }
            },
            Token { kind: TokenKind::StringLiteral, loc, text, ..} => {
                self.increment();
                Arc::new(AST::StringLiteral(loc, text))
            },
            Token { kind: TokenKind::Identifier, loc, text, ..} => {
                self.increment();
                Arc::new(AST::Variable(loc, text))
            },
            _ => error!("{}: Unexpected token in parse_atom: {}", self.cur().loc, self.cur())
        }
    }
}