use crate::{lexer, ast, utils::error};

pub struct Parser {
    tokens: Vec<lexer::Token>,
    current_index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<lexer::Token>) -> Parser {
        Parser {
            tokens,
            current_index: 0,
        }
    }

    fn cur(&self) -> lexer::Token {
        self.tokens.get(self.current_index).cloned().expect("Unexpected end of file")
    }

    fn increment(&mut self) {
        match self.cur() {
            lexer::Token::EOF(_) => {}
            _ => { self.current_index += 1; }
        }
    }

    pub fn parse(&mut self) -> Box<ast::AST> {
        match self.cur() {
            lexer::Token::EOF(_) => {},
            _ => error!("{}: Expected EOF", self.cur().location())
        }
        self.parse_expression()
    }

    fn parse_expression(&mut self) -> Box<ast::AST> {
        self.parse_additive()
    }

    fn parse_additive(&mut self) -> Box<ast::AST> {
        let mut left = self.parse_multiplicative();
        loop {
            match self.cur() {
                lexer::Token::Plus(_) | lexer::Token::Minus(_) => {
                    let op = self.cur().clone();
                    self.increment();
                    let right = self.parse_multiplicative();
                    left = match op {
                        lexer::Token::Plus(loc) => Box::new(ast::AST::Plus(loc, left, right)),
                        lexer::Token::Minus(loc) => Box::new(ast::AST::Minus(loc, left, right)),
                        _ => unreachable!()
                    }
                },
                _ => {return left;}
            }
        }
    }

    fn parse_multiplicative(&mut self) -> Box<ast::AST> {
        let mut left = self.parse_postfix();
        loop {
            match self.cur() {
                lexer::Token::Star(_) | lexer::Token::Slash(_) => {
                    let op = self.cur().clone();
                    self.increment();
                    let right = self.parse_postfix();
                    left = match op {
                        lexer::Token::Star(loc) => Box::new(ast::AST::Multiply(loc, left, right)),
                        lexer::Token::Slash(loc) => Box::new(ast::AST::Divide(loc, left, right)),
                        _ => unreachable!()
                    }
                },
                _ => {return left;}
            }
        }
    }

    fn parse_postfix(&mut self) -> Box<ast::AST> {
        self.parse_atom()
    }

    fn parse_atom(&mut self) -> Box<ast::AST> {
        match self.cur() {
            lexer::Token::LeftParen(_) => {
                self.increment();
                let expr = self.parse_expression();
                match self.cur() {
                    lexer::Token::RightParen(_) => {
                        self.increment();
                        expr
                    },
                    _ => error!("{}: Expected ')'", self.cur().location())
                }
            }
            lexer::Token::IntegerLiteral(loc, num) => {
                self.increment();
                Box::new(ast::AST::IntegerLiteral(loc, num))
            },
            lexer::Token::FloatLiteral(loc, num) => {
                self.increment();
                Box::new(ast::AST::FloatLiteral(loc, num))
            },
            lexer::Token::StringLiteral(loc, string) => {
                self.increment();
                Box::new(ast::AST::StringLiteral(loc, string))
            },
            _ => error!("{}: Unexpected token reached: {}", self.cur().location(), self.cur())
        }
    }
}