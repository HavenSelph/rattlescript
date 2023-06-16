use crate::ast::ArgumentType::{Keyword, Positional, VariadicKeyword};
use crate::ast::{ArgumentType, CallArgs, FunctionArgs, AST, ImportObject};
use crate::common::Span;
use crate::error::{eof_error, parser_error as error, Result};
use crate::token::{Token, TokenKind};
use std::collections::HashMap;
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

    fn parse_class(&mut self) -> Result<Rc<AST>> {
        let start = self.consume(TokenKind::Class)?.span;
        let name = self.consume(TokenKind::Identifier)?;
        let parents = if self.cur().kind == TokenKind::LeftParen {
            self.increment();
            let mut parents = vec![];
            while self.cur().kind != TokenKind::RightParen {
                parents.push(self.consume(TokenKind::Identifier)?.text);
                if self.cur().kind == TokenKind::Comma {
                    self.increment();
                } else {
                    break;
                }
            }
            self.consume(TokenKind::RightParen)?;
            Some(parents)
        } else {
            None
        };
        let mut fields: HashMap<String, (Rc<AST>, bool)> = HashMap::new();

        self.consume(TokenKind::LeftBrace)?;
        while self.cur().kind != TokenKind::RightBrace {
            let is_static = if self.cur().kind == TokenKind::Static {
                self.increment();
                true
            } else {
                false
            };
            match self.cur().kind {
                TokenKind::Class => {
                    let class = self.parse_class()?;
                    match class.as_ref() {
                        AST::Class { span, name, .. } => {
                            if fields.contains_key(name) {
                                error!(span, "Duplicate field name");
                            }
                            fields.insert(name.clone(), (class.clone(), is_static));
                        }
                        _ => unreachable!(),
                    }
                }
                TokenKind::Def => {
                    let func = self.parse_function(!is_static, is_static)?;
                    match func.as_ref() {
                        AST::Function { span, name, .. } => {
                            if fields.contains_key(name.clone().unwrap().as_str()) {
                                error!(span, "Duplicate field name");
                            }
                            fields.insert(name.clone().unwrap(), (func.clone(), is_static));
                        }
                        _ => unreachable!(),
                    }
                }
                TokenKind::Let => {
                    let assignment = self.parse_statement(TokenKind::RightBrace)?;
                    match assignment.as_ref() {
                        AST::VarDeclaration {
                            0: span,
                            1: lhs,
                            2: val,
                        } => {
                            if fields.contains_key(lhs.as_str()) {
                                error!(span, "Duplicate field name");
                            }
                            fields.insert(lhs.clone(), (val.clone(), is_static));
                        },
                        _ => unreachable!(),
                    }
                }
                _ => {
                    error!(self.cur().span, "Expected class or function declaration");
                }
            }
        }
        let end = self.consume(TokenKind::RightBrace)?.span;
        self.consume_line_end()?;
        Ok(Rc::new(AST::Class {
            span: start.extend(&end),
            name: name.text,
            parents,
            fields,
        }))
    }

    fn parse_lambda(&mut self) -> Result<Rc<AST>> {
        let start = self.consume(TokenKind::Pipe)?.span;
        let (args, required) = self.parse_function_arguments(&start, TokenKind::Pipe, false)?;
        self.consume(TokenKind::Pipe)?;
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
            required,
            is_static: false,
            in_class: false,
            body,
        }))
    }

    fn parse_function(&mut self, in_class: bool, is_static: bool) -> Result<Rc<AST>> {
        let start = self.consume(TokenKind::Def)?.span;
        let name = self.consume(TokenKind::Identifier)?;
        self.consume(TokenKind::LeftParen)?;
        let (args, required) =
            self.parse_function_arguments(&start, TokenKind::RightParen, in_class)?;
        self.consume(TokenKind::RightParen)?;
        let body = if self.cur().kind == TokenKind::FatArrow {
            self.increment();
            let hint = self.cur().kind == TokenKind::LeftBrace;
            let expr = match self.parse_expression() {
                Ok(expr) => expr,
                Err(err) => {
                    if hint {
                        error!(
                            err.span,
                            "Function expected expression, did you mean to use a block?"
                        );
                    } else {
                        return Err(err);
                    }
                }
            };
            Rc::new(AST::Return(*expr.span(), expr))
        } else {
            self.parse_block(/*global*/ false)?
        };
        Ok(Rc::new(AST::Function {
            span: start.extend(body.span()),
            name: Some(name.text),
            args,
            required,
            is_static,
            in_class,
            body,
        }))
    }

    fn parse_function_arguments(
        &mut self,
        span: &Span,
        closer: TokenKind,
        in_class: bool,
    ) -> Result<(FunctionArgs, usize)> {
        let mut accepting: Vec<ArgumentType> =
            vec![Positional, ArgumentType::Variadic, Keyword, VariadicKeyword];
        let mut args: FunctionArgs = vec![]; // (name, default, type)
        let mut required: usize = 0;
        let mut found_self = !in_class;

        while self.cur().kind != closer {
            let mut span = self.cur().span;
            // Todo: Change this to use StarExpression, StarStarExpression, AssignmentExpression, and Identifier AST nodes
            match self.cur().kind {
                TokenKind::Star => {
                    if accepting.contains(&ArgumentType::Variadic) {
                        self.increment();
                        accepting = vec![Keyword, VariadicKeyword];

                        let name = if self.cur().kind == TokenKind::Identifier {
                            span = span.extend(&self.cur().span);
                            let name = self.consume(TokenKind::Identifier)?.text;
                            Some(name)
                        } else {
                            None
                        };

                        args.push((name.unwrap_or("".to_string()), None, ArgumentType::Variadic));
                    } else {
                        error!(
                            self.cur().span,
                            "Positional variadic argument is not valid here"
                        );
                    }
                }
                TokenKind::StarStar => {
                    if accepting.contains(&VariadicKeyword) {
                        self.increment();
                        accepting = vec![];

                        let name = if self.cur().kind == TokenKind::Identifier {
                            span = span.extend(&self.cur().span);
                            let name = self.consume(TokenKind::Identifier)?.text;
                            Some(name)
                        } else {
                            None
                        };

                        args.push((name.unwrap_or("".to_string()), None, VariadicKeyword));
                    } else {
                        error!(
                            self.cur().span,
                            "Keyword variadic argument is not valid here"
                        );
                    }
                }
                _ => {
                    if accepting.contains(&Positional) || accepting.contains(&Keyword) {
                        let name = self.consume(TokenKind::Identifier)?.text;
                        if self.cur().kind == TokenKind::Equals {
                            span = span.extend(&self.cur().span);
                            self.increment();
                            accepting = vec![Keyword, VariadicKeyword];
                            args.push((name, Some(self.parse_expression()?), Keyword));
                        } else {
                            span = span.extend(&self.cur().span);
                            args.push((name, None, Positional));
                            required += 1;
                        };
                    } else {
                        error!(self.cur().span, "Unexpected argument");
                    }
                }
            }
            if !found_self && in_class && args[0].0 != "self" {
                error!(span, "First argument must be 'self'");
            } else if !found_self && in_class {
                found_self = true;
                args.pop();
            }
            if self.cur().kind == TokenKind::Comma {
                self.increment();
            } else {
                break;
            }
        }
        if in_class {
            if args.is_empty() && !found_self {
                error!(
                    span.extend(&self.cur().span),
                    "Class method must include 'self' as first argument"
                );
            } else {
                required -= 1;
            }
        }
        Ok((args, required))
    }

    fn parse_import_module(&mut self) -> Result<(String, Span)> {
        // Fixme: Imports do not work properly when CWD is not at the same dir of the file being imported
        let mut module: Vec<String> = Vec::new();
        let mut span = self.cur().span;
        loop {
            let text = self.consume(TokenKind::Identifier)?;
            span = span.extend(&text.span);
            module.push(text.text);
            if self.cur().kind == TokenKind::Dot {
                self.increment();
            } else {
                break;
            }
        }
        let mut path = "./".to_string();
        path.push_str(&module.join(std::path::MAIN_SEPARATOR.to_string().as_str()));
        path.push_str(".rat");
        Ok((
            path,
            span
        ))
    }

    fn parse_import_object(&mut self) -> Result<ImportObject> {
        let mut objects: Vec<(String, Option<String>)> = Vec::new();
        let mut span = self.cur().span;
        match self.cur().kind {
            TokenKind::LeftParen => {
                self.increment();
                loop {
                    let object = self.consume(TokenKind::Identifier)?.text;
                    let alias = if self.cur().kind == TokenKind::As {
                        self.increment();
                        Some(self.consume(TokenKind::Identifier)?.text)
                    } else {
                        None
                    };
                    objects.push((object, alias));
                    if self.cur().kind == TokenKind::Comma {
                        self.increment();
                    } else {
                        break;
                    }
                    span = span.extend(&self.cur().span);
                }
                self.consume(TokenKind::RightParen)?;
            }
            TokenKind::Identifier => {
                let object = self.consume(TokenKind::Identifier)?;
                span = span.extend(&object.span);
                let object = object.text;
                let alias = if self.cur().kind == TokenKind::As {
                    self.increment();
                    let name = self.consume(TokenKind::Identifier)?;
                    span = span.extend(&name.span);
                    Some(name.text)
                } else {
                    None
                };
                objects.push((object, alias));
            }
            _ => error!(self.cur().span, "Expected identifier or '('"),
        };
        Ok((objects, span))
    }

    fn parse_import(&mut self) -> Result<Rc<AST>> {
        let start = self.cur();
        self.increment();
        Ok(match start.kind {
            TokenKind::From => {
                let (module, _) = self.parse_import_module()?;
                self.consume(TokenKind::Import)?;
                let (objects, span) = self.parse_import_object()?;
                let span = start.span.extend(&span);
                self.consume_line_end()?;

                let path = std::path::Path::new(&module).with_extension("rat");
                if !path.exists() {
                    error!(span, "Module '{}' does not exist", module);
                }

                Rc::new(AST::FromImport {
                    span,
                    path: path.to_str().unwrap().to_string(),
                    names: objects,
                })
            }
            TokenKind::Import => {
                let module = self.parse_import_module()?;
                self.consume_line_end()?;
                let alias = if self.cur().kind == TokenKind::As {
                    self.increment();
                    Some(self.consume(TokenKind::Identifier)?.text)
                } else {
                    None
                };
                self.consume_line_end()?;

                let path = std::path::Path::new(&module.0);
                if !path.exists() {
                    error!(module.1, "Module '{}' does not exist", module.0);
                }

                Rc::new(AST::Import {
                    span: start.span.extend(&module.1),
                    path: path.to_str().unwrap().to_string(),
                    alias,
                })
            }
            _ => unreachable!("parse_import called without 'import' or 'from'"),
        })
    }

    fn parse_statement(&mut self, until: TokenKind) -> Result<Rc<AST>> {
        match self.cur() {
            Token {
                kind: TokenKind::Namespace,
                span,
                ..
            } => {
                self.increment();
                let ident = self.consume(TokenKind::Identifier)?;

                // Parse block of only functions, classes or variables
                let start = self.consume(TokenKind::LeftBrace)?.span;
                let mut body = Vec::new();
                loop {
                    match self.cur().kind {
                        TokenKind::Def => {
                            body.push(self.parse_function(false, false)?);
                        }
                        TokenKind::Class => {
                            body.push(self.parse_class()?);
                        }
                        TokenKind::Let => {
                            body.push(self.parse_statement(TokenKind::RightBrace)?);
                        }
                        TokenKind::RightBrace => {
                            break;
                        }
                        _ => {
                            error!(self.cur().span, "Expected function, class or variable");
                        }
                    }
                    while self.cur().kind == TokenKind::SemiColon {
                        self.increment();
                    }
                }
                let end = self.consume(TokenKind::RightBrace)?.span;
                let body = Rc::new(AST::Block(start.extend(&end), body));
                self.consume_line_end_until(until)?;
                Ok(Rc::new(AST::Namespace {
                    span: span.extend(body.span()),
                    name: ident.text,
                    body,
                }))
            }
            Token {
                kind: TokenKind::Import,
                ..
            }
            | Token {
                kind: TokenKind::From,
                ..
            } => self.parse_import(),
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
                    }
                }
            }
            Token {
                kind: TokenKind::Class,
                ..
            } => Ok(self.parse_class()?),
            Token {
                kind: TokenKind::Static,
                ..
            } => {
                error!(
                    self.cur().span,
                    "Static methods are not valid in this context."
                );
            }
            Token {
                kind: TokenKind::Def,
                ..
            } => Ok(self.parse_function(false, false)?),
            Token {
                kind: TokenKind::At,
                span,
                ..
            } => {
                self.increment();
                let deco = self.parse_postfix()?;
                self.consume_line_end()?;
                let func = self.parse_function(false, false)?;
                let name = match func.as_ref() {
                    AST::Function { name, .. } => name.clone(),
                    _ => unreachable!(),
                };
                self.consume_line_end_until(until)?;
                Ok(Rc::new(AST::Assignment(
                    span.extend(deco.span()),
                    Rc::new(AST::Variable(span.extend(deco.span()), name.unwrap())),
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
                let mut msg = None;
                if self.cur().kind == TokenKind::Comma {
                    self.increment();
                    span.extend(&self.cur().span);
                    msg = Some(self.consume(TokenKind::StringLiteral)?.text);
                }
                self.consume_line_end_until(until)?;
                Ok(Rc::new(AST::Assert(span, cond, msg)))
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
                let right = self.parse_assignment()?;
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
        let mut left = self.parse_multiplicative()?;
        while let Token {
            kind: TokenKind::Plus | TokenKind::Minus,
            ..
        } = self.cur()
        {
            let op = self.cur().kind;
            self.increment();
            let right = self.parse_multiplicative()?;
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

    fn parse_multiplicative(&mut self) -> Result<Rc<AST>> {
        let mut left = self.parse_exponential()?;

        while let Token {
            kind: TokenKind::Star | TokenKind::Slash | TokenKind::Percent,
            ..
        } = self.cur()
        {
            let op = self.cur().kind;
            self.increment();
            let right = self.parse_exponential()?;
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

    fn parse_exponential(&mut self) -> Result<Rc<AST>> {
        let mut left = self.parse_prefix()?;
        while let Token {
            kind: TokenKind::StarStar,
            ..
        } = self.cur()
        {
            let op = self.cur().kind;
            self.increment();
            let right = self.parse_exponential()?;
            left = match op {
                TokenKind::StarStar => {
                    Rc::new(AST::Power(left.span().extend(right.span()), left, right))
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
            TokenKind::Star {
                ..
            } => {
                let start = self.cur().span;
                self.increment();
                // Can't allow another prefix, so we parse a postfix
                let expr = self.parse_postfix()?;
                Ok(Rc::new(AST::StarExpression(
                    start.extend(expr.span()),
                    expr,
                )))
            }
            TokenKind::StarStar {
                ..
            } => {
                let start = self.cur().span;
                self.increment();
                // Can't allow another prefix, so we parse a postfix
                let expr = self.parse_postfix()?;
                Ok(Rc::new(AST::StarStarExpression(
                    start.extend(expr.span()),
                    expr,
                )))
            }
            _ => self.parse_postfix(),
        }
    }

    fn parse_postfix(&mut self) -> Result<Rc<AST>> {
        let mut val = self.parse_atom()?;
        if self.cur().newline_before {
            return Ok(val);
        }
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
                    let args = self.parse_call_arguments(TokenKind::RightParen)?;
                    let span = val
                        .span()
                        .extend(&self.consume(TokenKind::RightParen)?.span);
                    val = Rc::new(AST::Call(span, val, args));
                }
                Token {
                    kind: TokenKind::Dot,
                    ..
                } => {
                    while let Token {
                        kind: TokenKind::Dot,
                        ..
                    } = self.cur()
                    {
                        self.increment();
                        let name = self.consume(TokenKind::Identifier)?;
                        val = Rc::new(AST::FieldAccess(
                            val.span().extend(&name.span),
                            val,
                            name.text.clone(),
                        ));
                    }
                }
                Token {
                    kind: TokenKind::DotDot,
                    ..
                } => {
                    self.increment();
                    let end = self.parse_prefix()?;
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

    fn parse_call_arguments(&mut self, closer: TokenKind) -> Result<CallArgs> {
        let mut args: CallArgs = vec![]; // Vec<(Option<String>, Rc<AST>)>
        while self.cur().kind != closer {
            let lhs = self.parse_expression()?;
            if self.cur().kind == TokenKind::Colon {
                self.increment();
                let name = match lhs.as_ref() {
                    AST::Variable(_, name) => name.to_string(),
                    _ => error!(lhs.span(), "Expected identifier: key pair"),
                };
                args.push((Some(name), self.parse_expression()?));
            } else {
                args.push((None, lhs));
            };
            if self.cur().kind == TokenKind::Comma {
                self.increment();
            } else {
                break;
            }
        }
        Ok(args)
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

    fn parse_format_string(&mut self, span: Span, text: String) -> Result<Rc<AST>> {
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
