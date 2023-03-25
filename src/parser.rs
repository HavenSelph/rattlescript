use crate::ast::Ast;
use crate::token::{Token, TokenKind};
use crate::utils::{parser_error as error, Result};
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
            TokenKind::Eof => {}
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
        } else {
            error!(
                self.cur().loc,
                "Expected token {:?}, but got {:?}",
                kind,
                self.cur().kind
            );
        }
    }

    pub fn parse(&mut self) -> Result<Rc<Ast>> {
        let res = self.parse_block(/*global*/ true);
        self.consume(TokenKind::Eof)?;
        res
    }

    fn parse_block(&mut self, global: bool) -> Result<Rc<Ast>> {
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
            if global && self.cur().kind == TokenKind::Eof {
                break;
            }
            statements.push(self.parse_statement()?);
        }
        Ok(Rc::new(Ast::Block(loc, statements)))
    }

    fn consume_line_end(&mut self) -> Result<()> {
        if self.cur().newline_before {
            return Ok(());
        }
        match self.cur().kind {
            TokenKind::SemiColon => self.increment(),
            TokenKind::Eof => {}
            _ => error!(
                self.cur().loc,
                "Expected line end, but got {:?}",
                self.cur().kind
            ),
        }
        Ok(())
    }

    fn parse_lambda(&mut self) -> Result<Rc<Ast>> {
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
            Rc::new(Ast::Return(loc.clone(), self.parse_expression()?))
        } else {
            self.parse_block(/*global*/ false)?
        };
        Ok(Rc::new(Ast::Function {
            loc,
            name: None,
            args,
            body,
        }))
    }

    fn parse_function(&mut self) -> Result<(Rc<Ast>, String)> {
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
            Rc::new(Ast::Return(loc.clone(), self.parse_expression()?))
        } else {
            self.parse_block(/*global*/ false)?
        };
        self.consume_line_end()?;
        Ok((
            Rc::new(Ast::Function {
                loc,
                name: Some(name.text.clone()),
                args,
                body,
            }),
            name.text,
        ))
    }

    fn parse_statement(&mut self) -> Result<Rc<Ast>> {
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
                Ok(Rc::new(Ast::VarDeclaration(loc, ident.text, expr)))
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
                        Ok(Rc::new(Ast::If(loc, cond, body, Some(else_body))))
                    }
                    _ => Ok(Rc::new(Ast::If(loc, cond, body, None))),
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
                Ok(Rc::new(Ast::Assignment(
                    loc.clone(),
                    Rc::new(Ast::Variable(loc.clone(), name)),
                    Rc::new(Ast::Call(loc, deco, vec![func])),
                )))
            }
            Token {
                kind: TokenKind::Continue,
                loc,
                ..
            } => {
                self.increment();
                self.consume_line_end()?;
                Ok(Rc::new(Ast::Continue(loc)))
            }
            Token {
                kind: TokenKind::Break,
                loc,
                ..
            } => {
                self.increment();
                self.consume_line_end()?;
                Ok(Rc::new(Ast::Break(loc)))
            }
            Token {
                kind: TokenKind::While,
                loc,
                ..
            } => {
                self.increment();
                let cond = self.parse_expression()?;
                let body = self.parse_block(/*global*/ false)?;
                Ok(Rc::new(Ast::While(loc, cond, body)))
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
                Ok(Rc::new(Ast::For(loc, ident.text, expr, body)))
            }
            Token {
                kind: TokenKind::Return,
                loc,
                ..
            } => {
                self.increment();
                let expr = self.parse_expression()?;
                self.consume_line_end()?;
                Ok(Rc::new(Ast::Return(loc, expr)))
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
                    if self.cur().kind != TokenKind::StringLiteral {
                        error!(
                            self.cur().loc,
                            "Expected string literal, but got {:?}",
                            self.cur().kind
                        );
                    }
                    self.parse_expression()?;
                }
                self.consume_line_end()?;
                Ok(Rc::new(Ast::Assert(loc, cond)))
            }
            _ => {
                let expr = self.parse_expression();
                self.consume_line_end()?;
                expr
            }
        }
    }

    fn parse_expression(&mut self) -> Result<Rc<Ast>> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Rc<Ast>> {
        let left = self.parse_comparison()?;
        match self.cur() {
            Token {
                kind: TokenKind::Equals,
                loc,
                ..
            } => {
                self.increment();
                let right = self.parse_comparison()?;
                Ok(Rc::new(Ast::Assignment(loc, left, right)))
            }
            _ => Ok(left),
        }
    }

    fn parse_comparison(&mut self) -> Result<Rc<Ast>> {
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
                TokenKind::EqualsEquals => Rc::new(Ast::Equals(loc, left, right)),
                TokenKind::BangEquals => Rc::new(Ast::NotEquals(loc, left, right)),
                TokenKind::LessThan => Rc::new(Ast::LessThan(loc, left, right)),
                TokenKind::GreaterThan => Rc::new(Ast::GreaterThan(loc, left, right)),
                TokenKind::LessThanEquals => Rc::new(Ast::LessThanEquals(loc, left, right)),
                TokenKind::GreaterThanEquals => Rc::new(Ast::GreaterThanEquals(loc, left, right)),
                _ => unreachable!(),
            }
        }
        Ok(left)
    }

    fn parse_logical_or(&mut self) -> Result<Rc<Ast>> {
        let mut left = self.parse_logical_and()?;
        while let Token {
            kind: TokenKind::Or,
            loc,
            ..
        } = self.cur()
        {
            self.increment();
            let right = self.parse_logical_and()?;
            left = Rc::new(Ast::Or(loc, left, right));
        }
        Ok(left)
    }

    fn parse_logical_and(&mut self) -> Result<Rc<Ast>> {
        let mut left = self.parse_additive()?;
        while let Token {
            kind: TokenKind::And,
            loc,
            ..
        } = self.cur()
        {
            self.increment();
            let right = self.parse_additive()?;
            left = Rc::new(Ast::And(loc, left, right));
        }
        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<Rc<Ast>> {
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
                TokenKind::Plus => Rc::new(Ast::Plus(loc, left, right)),
                TokenKind::Minus => Rc::new(Ast::Minus(loc, left, right)),
                _ => unreachable!(),
            }
        }
        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<Rc<Ast>> {
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
                TokenKind::Star => Rc::new(Ast::Multiply(loc, left, right)),
                TokenKind::Slash => Rc::new(Ast::Divide(loc, left, right)),
                _ => unreachable!(),
            }
        }
        Ok(left)
    }

    fn parse_slice_value(&mut self) -> Result<Option<Rc<Ast>>> {
        match self.cur().kind {
            TokenKind::Colon | TokenKind::RightBracket => Ok(None),
            _ => Ok(Some(self.parse_expression()?)),
        }
    }

    fn parse_prefix(&mut self) -> Result<Rc<Ast>> {
        match self.cur().kind {
            TokenKind::Not => {
                let loc = self.cur().loc;
                self.increment();
                let expr = self.parse_prefix()?;
                Ok(Rc::new(Ast::Not(loc, expr)))
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Result<Rc<Ast>> {
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
                            val = Rc::new(Ast::Index(loc.clone(), val, start));
                            continue;
                        } else {
                            error!(loc, "Cannot have empty index");
                        }
                    }

                    self.consume(TokenKind::Colon)?;
                    let end = self.parse_slice_value()?;

                    if self.cur().kind == TokenKind::RightBracket {
                        self.increment();
                        val = Rc::new(Ast::Slice {
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
                    val = Rc::new(Ast::Slice {
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
                                    _ => error!(
                                        self.cur().loc,
                                        "Expected `)` or `,` but got {:?}",
                                        self.cur().kind
                                    ),
                                }
                            }
                        }
                    }
                    val = Rc::new(Ast::Call(loc, val, args));
                }
                Token {
                    kind: TokenKind::DotDot,
                    loc,
                    ..
                } => {
                    self.increment();
                    let end = self.parse_atom()?;
                    val = Rc::new(Ast::Range(loc, val, end));
                }
                _ => break,
            }
        }
        Ok(val)
    }

    fn parse_atom(&mut self) -> Result<Rc<Ast>> {
        match self.cur() {
            Token {
                kind: TokenKind::LeftParen,
                ..
            } => {
                self.increment();
                let expr = self.parse_expression()?;
                match self.cur().kind {
                    TokenKind::RightParen => {
                        self.increment();
                        Ok(expr)
                    }
                    _ => error!(self.cur().loc, "Expected `)` but got {:?}", self.cur().kind),
                }
            }
            Token {
                kind: TokenKind::Pipe,
                ..
            } => self.parse_lambda(),
            Token {
                kind: TokenKind::IntegerLiteral,
                loc,
                text,
                ..
            } => {
                self.increment();
                if let Ok(num) = text.parse::<i64>() {
                    Ok(Rc::new(Ast::IntegerLiteral(loc, num)))
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
                    Ok(Rc::new(Ast::FloatLiteral(loc, num)))
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
                Ok(Rc::new(Ast::StringLiteral(loc, text)))
            }
            Token {
                kind: TokenKind::Identifier,
                loc,
                text,
                ..
            } => {
                self.increment();
                Ok(Rc::new(Ast::Variable(loc, text)))
            }
            Token {
                kind: TokenKind::True,
                loc,
                ..
            } => {
                self.increment();
                Ok(Rc::new(Ast::BooleanLiteral(loc, true)))
            }
            Token {
                kind: TokenKind::False,
                loc,
                ..
            } => {
                self.increment();
                Ok(Rc::new(Ast::BooleanLiteral(loc, false)))
            }
            Token {
                kind: TokenKind::Nothing,
                loc,
                ..
            } => {
                self.increment();
                Ok(Rc::new(Ast::Nothing(loc)))
            }
            _ => error!(
                self.cur().loc,
                "Unexpected token in parse_atom: {}",
                self.cur()
            ),
        }
    }
}
