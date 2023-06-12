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
            eof_error!(self.cur().span, "Expected token {:?}", kind);
        } else {
            error!(
                self.cur().span,
                "Expected token {:?}, but got {:?}",
                kind,
                self.cur().kind
            );
        }
    }

    fn consume_line_end(&mut self) -> Result<()> {
        if self.cur().newline_before {
            return Ok(());
        }
        match self.cur().kind {
            TokenKind::SemiColon => self.increment(),
            TokenKind::EOF => {}
            _ => error!(
                self.cur().span,
                "Expected line end, but got {:?}",
                self.cur().kind
            ),
        }
        Ok(())
    }

    fn consume_line_end_until(&mut self, until: TokenKind) -> Result<()> {
        if self.cur().kind == until {
            return Ok(());
        }
        self.consume_line_end()
    }

    pub fn parse(&mut self) -> Result<Rc<AST>> {
        let res = self.parse_block(/*global*/ true)?;
        self.consume(TokenKind::EOF)?;
        Ok(res)
    }

    fn parse_block(&mut self, global: bool) -> Result<Rc<AST>> {
        let mut span = self.cur().span;
        let mut statements = vec![];
        let until = if !global {
            self.consume(TokenKind::LeftBrace)?;
            TokenKind::RightBrace
        } else {
            TokenKind::EOF
        };
        loop {
            if !global && self.cur().kind == TokenKind::RightBrace {
                span = span.extend(&self.cur().span);
                self.increment();
                break;
            }
            if global && self.cur().kind == TokenKind::EOF {
                span = span.extend(&self.cur().span);
                break;
            }
            statements.push(self.parse_statement(until.clone())?);
            while self.cur().kind == TokenKind::SemiColon {
                self.increment();
            }
        }
        Ok(Rc::new(AST::Block(span, statements)))
    }

    fn parse_class(&mut self) -> Result<(Rc<AST>, String)> {
        let start = self.consume(TokenKind::Class)?.span;
        let name = self.consume(TokenKind::Identifier)?;
        let mut fields: Vec<(String, Option<Rc<AST>>)> = vec![];
        let mut methods: Vec<(String, Rc<AST>)> = vec![];
        let mut comma = true;
        let mut default = false;
        self.consume(TokenKind::LeftBrace)?;
        loop {
            match self.cur().kind {
                TokenKind::RightBrace => {
                    if fields.is_empty() {
                        error!(self.cur().span, "Classes must have at least one field");
                    } else {
                        break;
                    }
                }
                TokenKind::Identifier => {
                    if !methods.is_empty() {
                        error!(self.cur().span, "Fields must be declared before methods");
                    } else if !comma {
                        error!(self.cur().span, "Expected comma after field declaration");
                    }
                    let name = self.consume(TokenKind::Identifier)?.text;
                    let default = if self.cur().kind == TokenKind::Equals {
                        default = true;
                        self.increment();
                        Some(self.parse_expression()?)
                    } else {
                        if default {
                            error!(self.cur().span, "Expected default value for field");
                        }
                        None
                    };
                    fields.push((name, default));
                    if self.cur().kind == TokenKind::Comma {
                        comma = true;
                        self.increment();
                    } else {
                        comma = false;
                    }
                }
                TokenKind::At => {
                    self.increment();
                    let deco = self.parse_postfix()?;
                    self.consume_line_end()?;
                    let (func, name) = self.parse_function()?;
                    self.consume_line_end()?;
                    let func = Rc::new(AST::Call(start.extend(deco.span()),deco,vec![(None, func)],));
                    methods.push((name, func));
                }
                TokenKind::Def => {
                    let (func, name) = self.parse_function()?;
                    methods.push((name, func));
                }
                TokenKind::EOF => {
                    eof_error!(self.cur().span, "Expected field or method declaration")
                }
                _ => error!(self.cur().span, "Expected field or method declaration"),
            }
        }
        let end = self.consume(TokenKind::RightBrace)?.span;
        Ok((
            Rc::new(AST::Class {
                span: start.extend(&end),
                name: name.text.clone(),
                fields,
                methods,
            }),
            name.text,
        ))
    }

    fn parse_lambda(&mut self) -> Result<Rc<AST>> {
        let start = self.consume(TokenKind::Pipe)?.span;
        let mut args = vec![];
        while self.cur().kind != TokenKind::Pipe {
            let name = self.consume(TokenKind::Identifier)?.text;
            let default = if self.cur().kind == TokenKind::Equals {
                self.increment();
                Some(self.parse_expression()?)
            } else {
                None
            };
            args.push((name, default));
            if self.cur().kind == TokenKind::Comma {
                self.increment();
            }
        }
        self.increment();
        let body = if self.cur().kind == TokenKind::FatArrow {
            self.increment();
            let expr = self.parse_expression()?;
            Rc::new(AST::Return(*expr.span(), expr))
        } else {
            self.parse_block(/*global*/ false)?
        };
        Ok(Rc::new(AST::Function {
            span: start.extend(body.span()),
            name: None,
            args,
            body,
        }))
    }

    fn parse_function(&mut self) -> Result<(Rc<AST>, String)> {
        let start = self.consume(TokenKind::Def)?.span;
        let name = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::LeftParen)?;
        let mut args = vec![];
        let mut seen_optional = false;
        while self.cur().kind != TokenKind::RightParen {
            let name = self.consume(TokenKind::Identifier)?.text;
            let default = if self.cur().kind == TokenKind::Equals {
                seen_optional = true;
                self.increment();
                Some(self.parse_expression()?)
            } else {
                if seen_optional {
                    error!(
                        self.cur().span,
                        "Required argument cannot follow optional argument"
                    );
                }
                None
            };
            args.push((name, default));
            if self.cur().kind == TokenKind::Comma {
                self.increment();
            }
        }
        self.increment();
        let body = if self.cur().kind == TokenKind::FatArrow {
            self.increment();
            let expr = self.parse_expression()?;
            self.consume_line_end()?;
            Rc::new(AST::Return(*expr.span(), expr))
        } else {
            self.parse_block(/*global*/ false)?
        };
        self.consume_line_end()?;
        Ok((
            Rc::new(AST::Function {
                span: start.extend(body.span()),
                name: Some(name.text.clone()),
                args,
                body,
            }),
            name.text,
        ))
    }

    fn parse_statement(&mut self, until: TokenKind) -> Result<Rc<AST>> {
        match self.cur() {
            Token {
                kind: TokenKind::Let,
                span,
                ..
            } => {
                self.increment();
                let ident = self.consume(TokenKind::Identifier)?;
                self.consume(TokenKind::Equals)?;
                let expr = self.parse_expression()?;
                self.consume_line_end_until(until)?;
                Ok(Rc::new(AST::VarDeclaration(
                    span.extend(expr.span()),
                    ident.text,
                    expr,
                )))
            }
            Token {
                kind: TokenKind::If,
                span,
                ..
            } => {
                self.increment();
                let cond = self.parse_expression()?;
                let body = self.parse_block(/*global*/ false)?;
                let span = span.extend(body.span());
                match self.cur() {
                    Token {
                        kind: TokenKind::Else,
                        span,
                        ..
                    } => {
                        self.increment();
                        let else_body = match self.cur().kind {
                            TokenKind::If => self.parse_statement(until)?,
                            _ => self.parse_block(/*global*/ false)?,
                        };
                        Ok(Rc::new(AST::If(
                            span.extend(else_body.span()),
                            cond,
                            body,
                            Some(else_body),
                        )))
                    }
                    _ => {
                        self.consume_line_end_until(until)?;
                        Ok(Rc::new(AST::If(span, cond, body, None)))
                    },
                }
            }
            Token {
                kind: TokenKind::Class,
                ..
            } => Ok(self.parse_class()?.0),
            Token {
                kind: TokenKind::Def,
                ..
            } => Ok(self.parse_function()?.0),
            Token {
                kind: TokenKind::At,
                span,
                ..
            } => {
                self.increment();
                let deco = self.parse_postfix()?;
                self.consume_line_end()?;
                let (func, name) = self.parse_function()?;
                self.consume_line_end_until(until)?;
                Ok(Rc::new(AST::Assignment(
                    span.extend(deco.span()),
                    Rc::new(AST::Variable(span.extend(deco.span()), name)),
                    Rc::new(AST::Call(
                        span.extend(deco.span()),
                        deco,
                        vec![(None, func)],
                    )),
                )))
            }
            Token {
                kind: TokenKind::Continue,
                span,
                ..
            } => {
                self.increment();
                self.consume_line_end_until(until)?;
                Ok(Rc::new(AST::Continue(span)))
            }
            Token {
                kind: TokenKind::Break,
                span,
                ..
            } => {
                self.increment();
                self.consume_line_end_until(until)?;
                Ok(Rc::new(AST::Break(span)))
            }
            Token {
                kind: TokenKind::While,
                span,
                ..
            } => {
                self.increment();
                let cond = self.parse_expression()?;
                let body = self.parse_block(/*global*/ false)?;
                Ok(Rc::new(AST::While(span.extend(body.span()), cond, body)))
            }
            Token {
                kind: TokenKind::For,
                span,
                ..
            } => {
                self.increment();

                if self.cur().kind == TokenKind::LeftParen {
                    // Traditional for loop
                    self.increment();
                    let init = if self.cur().kind == TokenKind::SemiColon {
                        None
                    } else {
                        let init = Some(self.parse_statement(TokenKind::SemiColon)?);
                        // If we have consumed a semicolon, we need to go back one token
                        if self.tokens[self.current_index - 1].kind == TokenKind::SemiColon {
                            self.current_index -= 1;
                        }
                        init
                    };
                    self.consume(TokenKind::SemiColon)?;
                    let cond = if self.cur().kind == TokenKind::SemiColon {
                        None
                    } else {
                        Some(self.parse_expression()?)
                    };
                    self.consume(TokenKind::SemiColon)?;
                    let step = if self.cur().kind == TokenKind::RightParen {
                        None
                    } else {
                        Some(self.parse_expression()?)
                    };
                    self.consume(TokenKind::RightParen)?;
                    let body = self.parse_block(/*global*/ false)?;
                    Ok(Rc::new(AST::For {
                        span: span.extend(body.span()),
                        init,
                        cond,
                        step,
                        body,
                    }))
                } else {
                    // For each loop
                    let ident = self.consume(TokenKind::Identifier)?;
                    self.consume(TokenKind::In)?;
                    let expr = self.parse_expression()?;
                    let body = self.parse_block(/*global*/ false)?;
                    Ok(Rc::new(AST::ForEach(
                        span.extend(body.span()),
                        ident.text,
                        expr,
                        body,
                    )))
                }
            }
            Token {
                kind: TokenKind::Return,
                span,
                ..
            } => {
                self.increment();
                let expr = self.parse_expression()?;
                self.consume_line_end_until(until)?;
                Ok(Rc::new(AST::Return(span.extend(expr.span()), expr)))
            }
            Token {
                kind: TokenKind::Assert,
                span,
                ..
            } => {
                self.increment();
                let cond = self.parse_expression()?;
                let span = span.extend(cond.span());
                if self.cur().kind == TokenKind::Comma {
                    self.increment();
                    span.extend(&self.cur().span);
                    self.consume(TokenKind::StringLiteral)?;
                }
                self.consume_line_end_until(until)?;
                Ok(Rc::new(AST::Assert(span, cond)))
            }
            _ => {
                let expr = self.parse_expression()?;
                self.consume_line_end_until(until)?;
                Ok(expr)
            }
        }
    }

    fn parse_expression(&mut self) -> Result<Rc<AST>> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Rc<AST>> {
        let left = self.parse_logical_or()?;
        match self.cur() {
            Token {
                kind: TokenKind::Equals,
                ..
            } => {
                self.increment();
                let right = self.parse_logical_or()?;
                Ok(Rc::new(AST::Assignment(
                    left.span().extend(right.span()),
                    left,
                    right,
                )))
            }
            Token {
                kind: TokenKind::PlusEquals,
                ..
            } => {
                self.increment();
                let right = self.parse_logical_or()?;
                Ok(Rc::new(AST::Assignment(
                    left.span().extend(right.span()),
                    left.clone(),
                    Rc::new(AST::Plus(left.span().extend(right.span()), left, right)),
                )))
            }
            Token {
                kind: TokenKind::MinusEquals,
                ..
            } => {
                self.increment();
                let right = self.parse_logical_or()?;
                Ok(Rc::new(AST::Assignment(
                    left.span().extend(right.span()),
                    left.clone(),
                    Rc::new(AST::Minus(left.span().extend(right.span()), left, right)),
                )))
            }
            Token {
                kind: TokenKind::StarEquals,
                ..
            } => {
                self.increment();
                let right = self.parse_logical_or()?;
                Ok(Rc::new(AST::Assignment(
                    left.span().extend(right.span()),
                    left.clone(),
                    Rc::new(AST::Multiply(left.span().extend(right.span()), left, right)),
                )))
            }
            Token {
                kind: TokenKind::SlashEquals,
                ..
            } => {
                self.increment();
                let right = self.parse_logical_or()?;
                Ok(Rc::new(AST::Assignment(
                    left.span().extend(right.span()),
                    left.clone(),
                    Rc::new(AST::Divide(left.span().extend(right.span()), left, right)),
                )))
            }
            _ => Ok(left),
        }
    }

    fn parse_logical_or(&mut self) -> Result<Rc<AST>> {
        let mut left = self.parse_logical_and()?;
        while let Token {
            kind: TokenKind::Or,
            ..
        } = self.cur()
        {
            self.increment();
            let right = self.parse_logical_and()?;
            left = Rc::new(AST::Or(left.span().extend(right.span()), left, right));
        }
        Ok(left)
    }

    fn parse_logical_and(&mut self) -> Result<Rc<AST>> {
        let mut left = self.parse_comparison()?;
        while let Token {
            kind: TokenKind::And,
            ..
        } = self.cur()
        {
            self.increment();
            let right = self.parse_comparison()?;
            left = Rc::new(AST::And(left.span().extend(right.span()), left, right));
        }
        Ok(left)
    }

    fn parse_comparison(&mut self) -> Result<Rc<AST>> {
        let mut left = self.parse_additive()?;
        while let Token {
            kind:
                TokenKind::EqualsEquals
                | TokenKind::BangEquals
                | TokenKind::LessThan
                | TokenKind::GreaterThan
                | TokenKind::LessEquals
                | TokenKind::GreaterEquals,
            ..
        } = self.cur()
        {
            let op = self.cur().kind;
            self.increment();
            let right = self.parse_additive()?;
            left = match op {
                TokenKind::EqualsEquals => {
                    Rc::new(AST::Equals(left.span().extend(right.span()), left, right))
                }
                TokenKind::BangEquals => Rc::new(AST::NotEquals(
                    left.span().extend(right.span()),
                    left,
                    right,
                )),
                TokenKind::LessThan => {
                    Rc::new(AST::LessThan(left.span().extend(right.span()), left, right))
                }
                TokenKind::GreaterThan => Rc::new(AST::GreaterThan(
                    left.span().extend(right.span()),
                    left,
                    right,
                )),
                TokenKind::LessEquals => Rc::new(AST::LessEquals(
                    left.span().extend(right.span()),
                    left,
                    right,
                )),
                TokenKind::GreaterEquals => Rc::new(AST::GreaterEquals(
                    left.span().extend(right.span()),
                    left,
                    right,
                )),
                _ => unreachable!(),
            }
        }
        Ok(left)
    }

    fn parse_additive(&mut self) -> Result<Rc<AST>> {
        let mut left = self.parse_exponential()?;
        while let Token {
            kind: TokenKind::Plus | TokenKind::Minus,
            ..
        } = self.cur()
        {
            let op = self.cur().kind;
            self.increment();
            let right = self.parse_exponential()?;
            left = match op {
                TokenKind::Plus => {
                    Rc::new(AST::Plus(left.span().extend(right.span()), left, right))
                }
                TokenKind::Minus => {
                    Rc::new(AST::Minus(left.span().extend(right.span()), left, right))
                }
                _ => unreachable!(),
            }
        }
        Ok(left)
    }

    fn parse_exponential(&mut self) -> Result<Rc<AST>> {
        let mut left = self.parse_multiplicative()?;
        while let Token {
            kind: TokenKind::StarStar,
            ..
        } = self.cur()
        {
            let op = self.cur().kind;
            self.increment();
            let right = self.parse_multiplicative()?;
            left = match op {
                TokenKind::StarStar => {
                    Rc::new(AST::Power(left.span().extend(right.span()), left, right))
                }
                _ => unreachable!(),
            }
        }
        Ok(left)
    }

    fn parse_multiplicative(&mut self) -> Result<Rc<AST>> {
        let mut left = self.parse_prefix()?;

        while let Token {
            kind:
                TokenKind::Star
                | TokenKind::StarStar
                | TokenKind::Slash
                | TokenKind::Percent,
            ..
        } = self.cur()
        {
            let op = self.cur().kind;
            self.increment();
            let right = self.parse_prefix()?;
            left = match op {
                TokenKind::Star => {
                    Rc::new(AST::Multiply(left.span().extend(right.span()), left, right))
                }
                TokenKind::Slash => {
                    Rc::new(AST::Divide(left.span().extend(right.span()), left, right))
                }
                TokenKind::Percent => {
                    Rc::new(AST::Modulo(left.span().extend(right.span()), left, right))
                }
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
            TokenKind::Minus => {
                let start = self.cur().span;
                self.increment();
                let expr = self.parse_prefix()?;
                Ok(Rc::new(AST::Negate(start.extend(expr.span()), expr)))
            }
            TokenKind::Not => {
                let start = self.cur().span;
                self.increment();
                let expr = self.parse_prefix()?;
                Ok(Rc::new(AST::Not(start.extend(expr.span()), expr)))
            }
            TokenKind::PlusPlus | TokenKind::MinusMinus => {
                let offset = if self.cur().kind == TokenKind::PlusPlus {
                    1
                } else {
                    -1
                };
                let start = self.cur().span;
                self.increment();
                let expr = self.parse_prefix()?;
                Ok(Rc::new(AST::PreIncrement(
                    start.extend(expr.span()),
                    expr,
                    offset,
                )))
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
                    ..
                } => {
                    self.increment();
                    let mut span = *val.span();

                    let start = self.parse_slice_value()?;
                    if self.cur().kind == TokenKind::RightBracket {
                        span = span.extend(&self.cur().span);
                        if let Some(start) = start {
                            self.increment();
                            val = Rc::new(AST::Index(span, val, start));
                            continue;
                        } else {
                            error!(span, "Cannot have empty index");
                        }
                    }

                    self.consume(TokenKind::Colon)?;
                    let end = self.parse_slice_value()?;

                    if self.cur().kind == TokenKind::RightBracket {
                        span = span.extend(&self.cur().span);
                        self.increment();
                        val = Rc::new(AST::Slice {
                            span,
                            lhs: val,
                            start,
                            end,
                            step: None,
                        });
                        continue;
                    }

                    self.consume(TokenKind::Colon)?;
                    let step = self.parse_slice_value()?;
                    span = span.extend(&self.cur().span);
                    self.consume(TokenKind::RightBracket)?;
                    val = Rc::new(AST::Slice {
                        span,
                        lhs: val,
                        start,
                        end,
                        step,
                    })
                }
                Token {
                    kind: TokenKind::LeftParen,
                    ..
                } => {
                    self.increment();
                    let mut args = vec![];
                    let mut span = *val.span();
                    loop {
                        match self.cur().kind {
                            TokenKind::RightParen => {
                                span = span.extend(&self.cur().span);
                                self.increment();
                                break;
                            }
                            _ => {
                                let mut arg = self.parse_expression()?;
                                let mut label = None;
                                if let AST::Variable(_, name) = &*arg {
                                    if self.cur().kind == TokenKind::Colon {
                                        label = Some(name.clone());
                                        self.increment();
                                        arg = self.parse_expression()?;
                                    }
                                }
                                args.push((label, arg));
                                match self.cur().kind {
                                    TokenKind::Comma => self.increment(),
                                    TokenKind::RightParen => {}
                                    TokenKind::EOF => eof_error!(
                                        self.cur().span,
                                        "Expected `)` or ',' but got EOF"
                                    ),
                                    _ => error!(
                                        self.cur().span,
                                        "Expected `)` or `,` but got {:?}",
                                        self.cur().kind
                                    ),
                                }
                            }
                        }
                    }
                    val = Rc::new(AST::Call(span, val, args));
                }
                Token {
                    kind: TokenKind::Dot,
                    ..
                } => {
                    self.increment();
                    let name = self.consume(TokenKind::Identifier)?;
                    val = Rc::new(AST::FieldAccess(
                        val.span().extend(&name.span),
                        val,
                        name.text.clone(),
                    ));
                }
                Token {
                    kind: TokenKind::DotDot,
                    ..
                } => {
                    self.increment();
                    let end = self.parse_atom()?;
                    val = Rc::new(AST::Range(val.span().extend(end.span()), val, end));
                }
                Token {
                    kind: TokenKind::PlusPlus | TokenKind::MinusMinus,
                    span,
                    ..
                } => {
                    let offset = if self.cur().kind == TokenKind::PlusPlus {
                        1
                    } else {
                        -1
                    };
                    self.increment();
                    val = Rc::new(AST::PostIncrement(val.span().extend(&span), val, offset));
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
                span,
                ..
            } => {
                self.increment();
                let mut exprs = vec![];
                let mut tup = false;
                while self.cur().kind != TokenKind::RightParen {
                    exprs.push(self.parse_expression()?);
                    match self.cur().kind {
                        TokenKind::Comma => {
                            self.increment();
                            tup = true;
                        }
                        TokenKind::RightParen => {}
                        TokenKind::EOF => {
                            eof_error!(self.cur().span, "Expected `)` or ',' but got EOF")
                        }
                        _ => error!(
                            self.cur().span,
                            "Expected `)` or `,` but got {:?}",
                            self.cur().kind
                        ),
                    }
                }
                let end = self.cur().span;
                self.consume(TokenKind::RightParen)?;
                match exprs.len() {
                    1 if !tup => Ok(exprs.pop().unwrap()),
                    _ => Ok(Rc::new(AST::TupleLiteral(span.extend(&end), exprs))),
                }
            }
            Token {
                kind: TokenKind::LeftBracket,
                span,
                ..
            } => {
                let mut arr = vec![];
                let mut comp = false;
                self.increment();
                while self.cur().kind != TokenKind::RightBracket {
                    arr.push(self.parse_expression()?);
                    if arr.len() == 1 && self.cur().kind == TokenKind::For {
                        comp = true;
                        break;
                    }
                    match self.cur().kind {
                        TokenKind::Comma => self.increment(),
                        TokenKind::RightBracket => {}
                        TokenKind::EOF => {
                            eof_error!(self.cur().span, "Expected `]` or ',' but got EOF")
                        }
                        _ => error!(
                            self.cur().span,
                            "Expected `]` or `,` but got {:?}",
                            self.cur().kind
                        ),
                    }
                }
                if comp {
                    self.consume(TokenKind::For)?;
                    let var = self.consume(TokenKind::Identifier)?;
                    self.consume(TokenKind::In)?;
                    let iter = self.parse_expression()?;
                    let cond = if self.cur().kind == TokenKind::If {
                        self.increment();
                        Some(self.parse_expression()?)
                    } else {
                        None
                    };
                    let end = self.consume(TokenKind::RightBracket)?.span;
                    Ok(Rc::new(AST::Comprehension(
                        span.extend(&end),
                        var.text,
                        iter,
                        arr.pop().unwrap(),
                        cond,
                    )))
                } else {
                    let end = self.consume(TokenKind::RightBracket)?.span;
                    Ok(Rc::new(AST::ArrayLiteral(span.extend(&end), arr)))
                }
            }
            Token {
                kind: TokenKind::LeftBrace,
                span,
                ..
            } => {
                self.increment();
                let mut items = vec![];
                while self.cur().kind != TokenKind::RightBrace {
                    let key = self.parse_expression()?;
                    self.consume(TokenKind::Colon)?;
                    let val = self.parse_expression()?;
                    items.push((key, val));
                    match self.cur().kind {
                        TokenKind::Comma => self.increment(),
                        TokenKind::RightBrace => {}
                        TokenKind::EOF => {
                            eof_error!(self.cur().span, "Expected `}}` or ',' but got EOF")
                        }
                        _ => error!(
                            self.cur().span,
                            "Expected `}}` or `,` but got {:?}",
                            self.cur().kind
                        ),
                    }
                }
                let end = self.consume(TokenKind::RightBrace)?.span;
                Ok(Rc::new(AST::DictionaryLiteral(span.extend(&end), items)))
            }
            Token {
                kind: TokenKind::Pipe,
                ..
            } => self.parse_lambda(),
            Token {
                kind: TokenKind::IntegerLiteralDec,
                span,
                text,
                ..
            } => {
                self.increment();
                if let Ok(num) = text.parse::<i64>() {
                    Ok(Rc::new(AST::IntegerLiteral(span, num)))
                } else {
                    error!(span, "Invalid integer literal: {}", text);
                }
            }
            Token {
                kind: TokenKind::IntegerLiteralBin,
                span,
                text,
                ..
            } => {
                self.increment();
                if let Ok(num) = i64::from_str_radix(&text, 2) {
                    Ok(Rc::new(AST::IntegerLiteral(span, num)))
                } else {
                    error!(span, "Invalid integer literal: {}", text);
                }
            }
            Token {
                kind: TokenKind::IntegerLiteralOct,
                span,
                text,
                ..
            } => {
                self.increment();
                if let Ok(num) = i64::from_str_radix(&text, 8) {
                    Ok(Rc::new(AST::IntegerLiteral(span, num)))
                } else {
                    error!(span, "Invalid integer literal: {}", text);
                }
            }
            Token {
                kind: TokenKind::IntegerLiteralHex,
                span,
                text,
                ..
            } => {
                self.increment();
                if let Ok(num) = i64::from_str_radix(&text, 16) {
                    Ok(Rc::new(AST::IntegerLiteral(span, num)))
                } else {
                    error!(span, "Invalid integer literal: {}", text);
                }
            }
            Token {
                kind: TokenKind::FloatLiteral,
                span,
                text,
                ..
            } => {
                self.increment();
                if let Ok(num) = text.parse::<f64>() {
                    Ok(Rc::new(AST::FloatLiteral(span, num)))
                } else {
                    error!(span, "Invalid float literal: {}", text);
                }
            }
            Token {
                kind: TokenKind::StringLiteral,
                span,
                text,
                ..
            } => {
                self.increment();
                Ok(Rc::new(AST::StringLiteral(span, text)))
            }
            Token {
                kind: TokenKind::FormatStringLiteral,
                span,
                text,
                ..
            } => {
                self.increment();
                self.parse_format_string(span, text)
            }
            Token {
                kind: TokenKind::Identifier,
                span,
                text,
                ..
            } => {
                self.increment();
                Ok(Rc::new(AST::Variable(span, text)))
            }
            Token {
                kind: TokenKind::True,
                span,
                ..
            } => {
                self.increment();
                Ok(Rc::new(AST::BooleanLiteral(span, true)))
            }
            Token {
                kind: TokenKind::False,
                span,
                ..
            } => {
                self.increment();
                Ok(Rc::new(AST::BooleanLiteral(span, false)))
            }
            Token {
                kind: TokenKind::Nothing,
                span,
                ..
            } => {
                self.increment();
                Ok(Rc::new(AST::Nothing(span)))
            }
            Token {
                kind: TokenKind::EOF,
                span,
                ..
            } => eof_error!(span, "Unexpected EOF in parse_atom"),
            _ => error!(
                self.cur().span,
                "Unexpected token in parse_atom: {}",
                self.cur()
            ),
        }
    }

    fn parse_format_string(&mut self, span: crate::common::Span, text: String) -> Result<Rc<AST>> {
        let mut parts = vec![];
        let mut buf = String::new();
        let mut start_index = 1;
        for (i, c) in text.chars().enumerate() {
            match c {
                '{' | '}' => {
                    parts.push((start_index, buf));
                    buf = String::new();
                    start_index = i + 2;
                }
                _ => {
                    buf.push(c);
                }
            }
        }
        parts.push((start_index, buf));
        let mut strings = vec![];
        let mut exprs: Vec<Rc<AST>> = vec![];
        for (index, (start_index, item)) in parts.iter().enumerate() {
            if index % 2 == 0 {
                strings.push(item.clone());
            } else {
                let mut lexer = crate::lexer::Lexer::new(item.clone(), span.0.filename);
                let mut location = span.0;
                location.column += start_index;
                lexer.location = location;
                let tokens = lexer.lex()?;
                let mut parser = Parser::new(tokens.clone());
                let expr = parser.parse_expression()?;
                match parser.consume(TokenKind::EOF) {
                    Ok(_) => {}
                    Err(e) => error!(e.span, "Invalid expression in format string"),
                }
                exprs.push(expr);
            }
        }
        Ok(Rc::new(AST::FormatStringLiteral(span, strings, exprs)))
    }
}
