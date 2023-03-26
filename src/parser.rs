use crate::ast::AST;
use crate::error::{eof_error, parser_error as error, Result};
use crate::token::{Token, TokenKind};
use std::rc::Rc;

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
        self.tokens
            .get(self.current_index)
            .cloned()
            .expect("should never move past end of file")
    }

    fn increment(&mut self) {
        match self.cur().kind {
            TokenKind::EOF => {}
            _ => {
                self.current_index += 1;
            }
        }
    }

    fn consume(&mut self, kind: TokenKind) -> Result<Token> {
        if self.cur().kind == kind {
            let res = self.cur();
            self.increment();
            Ok(res)
        } else if self.cur().kind == TokenKind::EOF {
            eof_error!(self.cur().loc, "Expected token {:?}", kind);
        } else {
            error!(
                self.cur().loc,
                "Expected token {:?}, but got {:?}",
                kind,
                self.cur().kind
            );
        }
    }

    pub fn parse(&mut self) -> Result<Rc<AST>> {
        match self.parse_block(/*global*/ true) {
            Ok(ast) => {
                self.consume(TokenKind::EOF)?;  // should never fail, but maybe there's an edge case?
                Ok(ast)
            },  // If it's already an error, don't print another one
            Err(e) => {
                Err(e)
            }
        }
    }

    fn parse_block(&mut self, global: bool) -> Result<Rc<AST>> {
        let loc = self.cur().loc;
        let mut statements = vec![];
        if !global {
            self.consume(TokenKind::LeftBrace)?;
        }
        loop {
            if !global && self.cur().kind == TokenKind::RightBrace {
                self.increment();
                break;
            }
            if global && self.cur().kind == TokenKind::EOF {
                break;
            }
            statements.push(self.parse_statement()?);
        }
        Ok(Rc::new(AST::Block(loc, statements)))
    }

    fn consume_line_end(&mut self) -> Result<()> {
        if self.cur().newline_before {
            return Ok(());
        }
        match self.cur().kind {
            TokenKind::SemiColon => self.increment(),
            TokenKind::EOF => {}
            _ => error!(
                self.cur().loc,
                "Expected line end, but got {:?}",
                self.cur().kind
            ),
        }
        Ok(())
    }

    fn parse_lambda(&mut self) -> Result<Rc<AST>> {
        let loc = self.consume(TokenKind::Pipe)?.loc;
        let mut args = vec![];
        while self.cur().kind != TokenKind::Pipe {
            args.push(self.consume(TokenKind::Identifier)?.text);
            if self.cur().kind == TokenKind::Comma {
                self.increment();
            }
        }
        self.increment();
        let body = if self.cur().kind == TokenKind::FatArrow {
            self.increment();
            Rc::new(AST::Return(loc.clone(), self.parse_expression()?))
        } else {
            self.parse_block(/*global*/ false)?
        };
        Ok(Rc::new(AST::Function {
            loc,
            name: None,
            args,
            body,
        }))
    }

    fn parse_function(&mut self) -> Result<(Rc<AST>, String)> {
        let loc = self.consume(TokenKind::Def)?.loc;
        let name = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::LeftParen)?;
        let mut args = vec![];
        while self.cur().kind != TokenKind::RightParen {
            args.push(self.consume(TokenKind::Identifier)?.text);
            if self.cur().kind == TokenKind::Comma {
                self.increment();
            }
        }
        self.increment();
        let body = if self.cur().kind == TokenKind::FatArrow {
            self.increment();
            Rc::new(AST::Return(loc.clone(), self.parse_expression()?))
        } else {
            self.parse_block(/*global*/ false)?
        };
        self.consume_line_end()?;
        Ok((
            Rc::new(AST::Function {
                loc,
                name: Some(name.text.clone()),
                args,
                body,
            }),
            name.text,
        ))
    }

    fn parse_statement(&mut self) -> Result<Rc<AST>> {
        match self.cur() {
            Token {
                kind: TokenKind::Let,
                loc,
                ..
            } => {
                self.increment();
                let ident = self.consume(TokenKind::Identifier)?;
                self.consume(TokenKind::Equals)?;
                let expr = self.parse_expression()?;
                self.consume_line_end()?;
                Ok(Rc::new(AST::VarDeclaration(loc, ident.text, expr)))
            }
            Token {
                kind: TokenKind::If,
                loc,
                ..
            } => {
                self.increment();
                let cond = self.parse_expression()?;
                let body = self.parse_block(/*global*/ false)?;
                match self.cur() {
                    Token {
                        kind: TokenKind::Else,
                        loc,
                        ..
                    } => {
                        self.increment();
                        let else_body = match self.cur().kind {
                            TokenKind::If => self.parse_statement()?,
                            _ => self.parse_block(/*global*/ false)?,
                        };
                        Ok(Rc::new(AST::If(loc, cond, body, Some(else_body))))
                    }
                    _ => Ok(Rc::new(AST::If(loc, cond, body, None))),
                }
            }
            Token {
                kind: TokenKind::Def,
                ..
            } => Ok(self.parse_function()?.0),
            Token {
                kind: TokenKind::At,
                loc,
                ..
            } => {
                self.increment();
                let deco = self.parse_postfix()?;
                self.consume_line_end()?;
                let (func, name) = self.parse_function()?;
                self.consume_line_end()?;
                Ok(Rc::new(AST::Assignment(
                    loc.clone(),
                    Rc::new(AST::Variable(loc.clone(), name)),
                    Rc::new(AST::Call(loc, deco, vec![func])),
                )))
            }
            Token {
                kind: TokenKind::Continue,
                loc,
                ..
            } => {
                self.increment();
                self.consume_line_end()?;
                Ok(Rc::new(AST::Continue(loc)))
            }
            Token {
                kind: TokenKind::Break,
                loc,
                ..
            } => {
                self.increment();
                self.consume_line_end()?;
                Ok(Rc::new(AST::Break(loc)))
            }
            Token {
                kind: TokenKind::While,
                loc,
                ..
            } => {
                self.increment();
                let cond = self.parse_expression()?;
                let body = self.parse_block(/*global*/ false)?;
                Ok(Rc::new(AST::While(loc, cond, body)))
            }
            Token {
                kind: TokenKind::For,
                loc,
                ..
            } => {
                self.increment();
                let ident = self.consume(TokenKind::Identifier)?;
                self.consume(TokenKind::In)?;
                let expr = self.parse_expression()?;
                let body = self.parse_block(/*global*/ false)?;
                Ok(Rc::new(AST::For(loc, ident.text, expr, body)))
            }
            Token {
                kind: TokenKind::Return,
                loc,
                ..
            } => {
                self.increment();
                let expr = self.parse_expression()?;
                self.consume_line_end()?;
                Ok(Rc::new(AST::Return(loc, expr)))
            }
            Token {
                kind: TokenKind::Assert,
                loc,
                ..
            } => {
                self.increment();
                let cond = self.parse_expression()?;
                if self.cur().kind == TokenKind::Comma {
                    self.increment();
                    self.consume(TokenKind::StringLiteral)?;
                }
                self.consume_line_end()?;
                Ok(Rc::new(AST::Assert(loc, cond)))
            }
            _ => {
                let expr = self.parse_expression();
                self.consume_line_end()?;
                expr
            }
        }
    }

    fn parse_expression(&mut self) -> Result<Rc<AST>> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Rc<AST>> {
        let left = self.parse_comparison()?;
        match self.cur() {
            Token {
                kind: TokenKind::Equals,
                loc,
                ..
            } => {
                self.increment();
                let right = self.parse_comparison()?;
                Ok(Rc::new(AST::Assignment(loc, left, right)))
            }
            _ => Ok(left),
        }
    }

    fn parse_comparison(&mut self) -> Result<Rc<AST>> {
        let mut left = self.parse_logical_or()?;
        while let Token {
            kind:
                TokenKind::EqualsEquals
                | TokenKind::BangEquals
                | TokenKind::LessThan
                | TokenKind::GreaterThan
                | TokenKind::LessThanEquals
                | TokenKind::GreaterThanEquals,
            loc,
            ..
        } = self.cur()
        {
            let op = self.cur().kind;
            self.increment();
            let right = self.parse_logical_or()?;
            left = match op {
                TokenKind::EqualsEquals => Rc::new(AST::Equals(loc, left, right)),
                TokenKind::BangEquals => Rc::new(AST::NotEquals(loc, left, right)),
                TokenKind::LessThan => Rc::new(AST::LessThan(loc, left, right)),
                TokenKind::GreaterThan => Rc::new(AST::GreaterThan(loc, left, right)),
                TokenKind::LessThanEquals => Rc::new(AST::LessThanEquals(loc, left, right)),
                TokenKind::GreaterThanEquals => Rc::new(AST::GreaterThanEquals(loc, left, right)),
                _ => unreachable!(),
            }
        }
        Ok(left)
    }

    fn parse_logical_or(&mut self) -> Result<Rc<AST>> {
        let mut left = self.parse_logical_and()?;
        while let Token {
            kind: TokenKind::Or,
            loc,
            ..
        } = self.cur()
        {
            self.increment();
            let right = self.parse_logical_and()?;
            left = Rc::new(AST::Or(loc, left, right));
        }
        Ok(left)
    }

    fn parse_logical_and(&mut self) -> Result<Rc<AST>> {
        let mut left = self.parse_additive()?;
        while let Token {
            kind: TokenKind::And,
            loc,
            ..
        } = self.cur()
        {
            self.increment();
            let right = self.parse_additive()?;
            left = Rc::new(AST::And(loc, left, right));
        }
        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<Rc<AST>> {
        let mut left = self.parse_multiplicative()?;
        while let Token {
            kind: TokenKind::Plus | TokenKind::Minus,
            loc,
            ..
        } = self.cur()
        {
            let op = self.cur().kind;
            self.increment();
            let right = self.parse_multiplicative()?;
            left = match op {
                TokenKind::Plus => Rc::new(AST::Plus(loc, left, right)),
                TokenKind::Minus => Rc::new(AST::Minus(loc, left, right)),
                _ => unreachable!(),
            }
        }
        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<Rc<AST>> {
        let mut left = self.parse_prefix()?;

        while let Token {
            kind: TokenKind::Star | TokenKind::Slash,
            loc,
            ..
        } = self.cur()
        {
            let op = self.cur().kind;
            self.increment();
            let right = self.parse_prefix()?;
            left = match op {
                TokenKind::Star => Rc::new(AST::Multiply(loc, left, right)),
                TokenKind::Slash => Rc::new(AST::Divide(loc, left, right)),
                _ => unreachable!(),
            }
        }
        Ok(left)
    }

    fn parse_slice_value(&mut self) -> Result<Option<Rc<AST>>> {
        match self.cur().kind {
            TokenKind::Colon | TokenKind::RightBracket => Ok(None),
            _ => Ok(Some(self.parse_expression()?)),
        }
    }

    fn parse_prefix(&mut self) -> Result<Rc<AST>> {
        match self.cur().kind {
            TokenKind::Not => {
                let loc = self.cur().loc;
                self.increment();
                let expr = self.parse_prefix()?;
                Ok(Rc::new(AST::Not(loc, expr)))
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Result<Rc<AST>> {
        let mut val = self.parse_atom()?;
        loop {
            match self.cur() {
                Token {
                    kind: TokenKind::LeftBracket,
                    loc,
                    ..
                } => {
                    self.increment();

                    let start = self.parse_slice_value()?;
                    if self.cur().kind == TokenKind::RightBracket {
                        if let Some(start) = start {
                            self.increment();
                            val = Rc::new(AST::Index(loc.clone(), val, start));
                            continue;
                        } else {
                            error!(loc, "Cannot have empty index");
                        }
                    }

                    self.consume(TokenKind::Colon)?;
                    let end = self.parse_slice_value()?;

                    if self.cur().kind == TokenKind::RightBracket {
                        self.increment();
                        val = Rc::new(AST::Slice {
                            loc: loc.clone(),
                            lhs: val,
                            start,
                            end,
                            step: None,
                        });
                        continue;
                    }

                    self.consume(TokenKind::Colon)?;
                    let step = self.parse_slice_value()?;
                    self.consume(TokenKind::RightBracket)?;
                    val = Rc::new(AST::Slice {
                        loc,
                        lhs: val,
                        start,
                        end,
                        step,
                    })
                }
                Token {
                    kind: TokenKind::LeftParen,
                    loc,
                    ..
                } => {
                    self.increment();
                    let mut args = vec![];
                    loop {
                        match self.cur().kind {
                            TokenKind::RightParen => {
                                self.increment();
                                break;
                            }
                            _ => {
                                args.push(self.parse_expression()?);
                                match self.cur().kind {
                                    TokenKind::Comma => self.increment(),
                                    TokenKind::RightParen => {}
                                    TokenKind::EOF => eof_error!(
                                        self.cur().loc,
                                        "Expected `)` or ',' but got EOF"
                                    ),
                                    _ => error!(
                                        self.cur().loc,
                                        "Expected `)` or `,` but got {:?}",
                                        self.cur().kind
                                    ),
                                }
                            }
                        }
                    }
                    val = Rc::new(AST::Call(loc, val, args));
                }
                Token {
                    kind: TokenKind::DotDot,
                    loc,
                    ..
                } => {
                    self.increment();
                    let end = self.parse_atom()?;
                    val = Rc::new(AST::Range(loc, val, end));
                }
                _ => break,
            }
        }
        Ok(val)
    }

    fn parse_atom(&mut self) -> Result<Rc<AST>> {
        match self.cur() {
            Token {
                kind: TokenKind::LeftParen,
                ..
            } => {
                self.increment();
                let expr = self.parse_expression()?;
                self.consume(TokenKind::RightParen)?;
                Ok(expr)
            }
            Token {
                kind: TokenKind::Pipe,
                ..
            } => self.parse_lambda(),
            Token {
                kind: TokenKind::IntegerLiteralDec,
                loc,
                text,
                ..
            } => {
                self.increment();
                if let Ok(num) = text.parse::<i64>() {
                    Ok(Rc::new(AST::IntegerLiteral(loc, num)))
                } else {
                    error!(loc, "Invalid integer literal: {}", text);
                }
            }
            Token {
                kind: TokenKind::IntegerLiteralBin,
                loc,
                text,
                ..
            } => {
                self.increment();
                if let Ok(num) = i64::from_str_radix(&text, 2) {
                    Ok(Rc::new(AST::IntegerLiteral(loc, num)))
                } else {
                    error!(loc, "Invalid integer literal: {}", text);
                }
            }
            Token {
                kind: TokenKind::IntegerLiteralOct,
                loc,
                text,
                ..
            } => {
                self.increment();
                if let Ok(num) = i64::from_str_radix(&text, 8) {
                    Ok(Rc::new(AST::IntegerLiteral(loc, num)))
                } else {
                    error!(loc, "Invalid integer literal: {}", text);
                }
            }
            Token {
                kind: TokenKind::IntegerLiteralHex,
                loc,
                text,
                ..
            } => {
                self.increment();
                if let Ok(num) = i64::from_str_radix(&text, 16) {
                    Ok(Rc::new(AST::IntegerLiteral(loc, num)))
                } else {
                    error!(loc, "Invalid integer literal: {}", text);
                }
            }
            Token {
                kind: TokenKind::FloatLiteral,
                loc,
                text,
                ..
            } => {
                self.increment();
                if let Ok(num) = text.parse::<f64>() {
                    Ok(Rc::new(AST::FloatLiteral(loc, num)))
                } else {
                    error!(loc, "Invalid float literal: {}", text);
                }
            }
            Token {
                kind: TokenKind::StringLiteral,
                loc,
                text,
                ..
            } => {
                self.increment();
                Ok(Rc::new(AST::StringLiteral(loc, text)))
            }
            Token {
                kind: TokenKind::Identifier,
                loc,
                text,
                ..
            } => {
                self.increment();
                Ok(Rc::new(AST::Variable(loc, text)))
            }
            Token {
                kind: TokenKind::True,
                loc,
                ..
            } => {
                self.increment();
                Ok(Rc::new(AST::BooleanLiteral(loc, true)))
            }
            Token {
                kind: TokenKind::False,
                loc,
                ..
            } => {
                self.increment();
                Ok(Rc::new(AST::BooleanLiteral(loc, false)))
            }
            Token {
                kind: TokenKind::Nothing,
                loc,
                ..
            } => {
                self.increment();
                Ok(Rc::new(AST::Nothing(loc)))
            }
            Token {
                kind: TokenKind::EOF,
                loc,
                ..
            } => eof_error!(loc, "Unexpected EOF in parse_atom"),
            _ => error!(
                self.cur().loc,
                "Unexpected token in parse_atom: {}",
                self.cur()
            ),
        }
    }
}
