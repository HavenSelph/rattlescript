use crate::common::Span;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ArgumentType {
    Positional,
    Variadic,
    Keyword,
    VariadicKeyword,
}

impl std::fmt::Display for ArgumentType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            ArgumentType::Positional => write!(f, "positional"),
            ArgumentType::Variadic => write!(f, "positional variadic"),
            ArgumentType::Keyword => write!(f, "keyword"),
            ArgumentType::VariadicKeyword => write!(f, "keyword variadic"),
        }
    }
}

pub type FunctionArgs = Vec<(String, Option<Rc<AST>>, ArgumentType)>;
pub type CallArgs = Vec<(Option<String>, Rc<AST>)>;
pub type ImportObject = (Vec<(String, Option<String>)>, Span);

#[derive(Debug)]
pub enum AST {
    And(Span, Rc<AST>, Rc<AST>),
    Assert(Span, Rc<AST>, Option<String>),
    Assignment(Span, Rc<AST>, Rc<AST>),
    Block(Span, Vec<Rc<AST>>),
    Class {
        span: Span,
        name: String,
        parents: Option<Vec<String>>,
        fields: HashMap<String, (Rc<AST>, bool)>,
    },
    BooleanLiteral(Span, bool),
    Call(Span, Rc<AST>, CallArgs),
    Divide(Span, Rc<AST>, Rc<AST>),
    Modulo(Span, Rc<AST>, Rc<AST>),
    FloatLiteral(Span, f64),
    Function {
        span: Span,
        name: Option<String>,
        args: FunctionArgs,
        required: usize,
        is_static: bool,
        in_class: bool,
        body: Rc<AST>,
    },
    Namespace {
        span: Span,
        name: String,
        body: Rc<AST>,
    },
    If(Span, Rc<AST>, Rc<AST>, Option<Rc<AST>>),
    Index(Span, Rc<AST>, Rc<AST>),
    IntegerLiteral(Span, i64),
    Minus(Span, Rc<AST>, Rc<AST>),
    Multiply(Span, Rc<AST>, Rc<AST>),
    Power(Span, Rc<AST>, Rc<AST>),
    Not(Span, Rc<AST>),
    Negate(Span, Rc<AST>),
    Nothing(Span),
    Or(Span, Rc<AST>, Rc<AST>),
    Plus(Span, Rc<AST>, Rc<AST>),
    Return(Span, Rc<AST>),
    Slice {
        span: Span,
        lhs: Rc<AST>,
        start: Option<Rc<AST>>,
        end: Option<Rc<AST>>,
        step: Option<Rc<AST>>,
    },
    StringLiteral(Span, String),
    VarDeclaration(Span, String, Rc<AST>),
    Variable(Span, String),
    Equals(Span, Rc<AST>, Rc<AST>),
    NotEquals(Span, Rc<AST>, Rc<AST>),
    LessThan(Span, Rc<AST>, Rc<AST>),
    GreaterThan(Span, Rc<AST>, Rc<AST>),
    LessEquals(Span, Rc<AST>, Rc<AST>),
    GreaterEquals(Span, Rc<AST>, Rc<AST>),
    While(Span, Rc<AST>, Rc<AST>),
    Continue(Span),
    Break(Span),
    ForEach(Span, String, Rc<AST>, Rc<AST>),
    For {
        span: Span,
        init: Option<Rc<AST>>,
        cond: Option<Rc<AST>>,
        step: Option<Rc<AST>>,
        body: Rc<AST>,
    },
    Import {
        span: Span,
        path: String,
        alias: Option<String>,
    },
    FromImport {
        span: Span,
        path: String,
        names: Vec<(String, Option<String>)>,
    },
    FieldAccess(Span, Rc<AST>, String),
    Comprehension(Span, String, Rc<AST>, Rc<AST>, Option<Rc<AST>>),
    FormatStringLiteral(Span, Vec<String>, Vec<Rc<AST>>),
    Range(Span, Rc<AST>, Rc<AST>),
    StarExpression(Span, Rc<AST>),
    StarStarExpression(Span, Rc<AST>),

    PostIncrement(Span, Rc<AST>, i64),
    PreIncrement(Span, Rc<AST>, i64),
    ArrayLiteral(Span, Vec<Rc<AST>>),
    TupleLiteral(Span, Vec<Rc<AST>>),
    DictionaryLiteral(Span, Vec<(Rc<AST>, Rc<AST>)>),
}

impl AST {
    pub fn span(&self) -> &Span {
        match self {
            AST::And(span, ..) => span,
            AST::Assert(span, ..) => span,
            AST::Assignment(span, ..) => span,
            AST::Block(span, ..) => span,
            AST::BooleanLiteral(span, ..) => span,
            AST::Call(span, ..) => span,
            AST::Class { span, .. } => span,
            AST::Divide(span, ..) => span,
            AST::FloatLiteral(span, ..) => span,
            AST::Function { span, .. } => span,
            AST::If(span, ..) => span,
            AST::Index(span, ..) => span,
            AST::IntegerLiteral(span, ..) => span,
            AST::Minus(span, ..) => span,
            AST::Multiply(span, ..) => span,
            AST::Modulo(span, ..) => span,
            AST::Power(span, ..) => span,
            AST::Not(span, ..) => span,
            AST::Nothing(span, ..) => span,
            AST::Negate(span, ..) => span,
            AST::Or(span, ..) => span,
            AST::Plus(span, ..) => span,
            AST::Return(span, ..) => span,
            AST::Slice { span, .. } => span,
            AST::StringLiteral(span, ..) => span,
            AST::VarDeclaration(span, ..) => span,
            AST::Variable(span, ..) => span,
            AST::Equals(span, ..) => span,
            AST::NotEquals(span, ..) => span,
            AST::LessThan(span, ..) => span,
            AST::GreaterThan(span, ..) => span,
            AST::LessEquals(span, ..) => span,
            AST::GreaterEquals(span, ..) => span,
            AST::While(span, ..) => span,
            AST::Continue(span, ..) => span,
            AST::Break(span, ..) => span,
            AST::ForEach(span, ..) => span,
            AST::For { span, .. } => span,
            AST::FieldAccess(span, ..) => span,
            AST::Comprehension(span, ..) => span,
            AST::FormatStringLiteral(span, ..) => span,
            AST::Range(span, ..) => span,
            AST::PostIncrement(span, ..) => span,
            AST::PreIncrement(span, ..) => span,
            AST::ArrayLiteral(span, ..) => span,
            AST::TupleLiteral(span, ..) => span,
            AST::DictionaryLiteral(span, ..) => span,
            AST::Import { span, .. } => span,
            AST::FromImport { span, .. } => span,
            AST::Namespace { span, .. } => span,
            AST::StarExpression(span, ..) => span,
            AST::StarStarExpression(span, ..) => span,
        }
    }
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            AST::And(_, lhs, rhs) => write!(f, "({} and {})", lhs, rhs),
            AST::Assert(_, expr, _) => write!(f, "assert {}", expr),
            AST::Assignment(_, lhs, rhs) => write!(f, "{} = {}", lhs, rhs),
            AST::Block(_, exprs) => write!(f, "<block with {} exprs>", exprs.len()),
            AST::BooleanLiteral(_, val) => write!(f, "{}", val),
            AST::Call(_, func, args) => {
                write!(f, "{}(", func)?;
                for (i, (_, arg)) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            AST::Class { name, .. } => write!(f, "<cls {}>", name,),
            AST::Divide(_, lhs, rhs) => write!(f, "({} / {})", lhs, rhs),
            AST::Modulo(_, lhs, rhs) => write!(f, "({} % {})", lhs, rhs),
            AST::FloatLiteral(_, val) => write!(f, "{}", val),
            AST::Function { name, .. } => write!(
                f,
                "def {} => ...",
                name.clone().unwrap_or_else(|| "<anon>".to_string())
            ),
            AST::If(_, cond, ..) => write!(f, "if {}", cond),
            AST::Index(_, lhs, rhs) => write!(f, "{}[{}]", lhs, rhs),
            AST::IntegerLiteral(_, val) => write!(f, "{}", val),
            AST::Minus(_, lhs, rhs) => write!(f, "({} - {})", lhs, rhs),
            AST::Multiply(_, lhs, rhs) => write!(f, "({} * {})", lhs, rhs),
            AST::Power(_, lhs, rhs) => write!(f, "({} ** {})", lhs, rhs),
            AST::Not(_, expr) => write!(f, "not {}", expr),
            AST::Nothing(_) => write!(f, "nothing"),
            AST::Negate(_, expr) => write!(f, "-{}", expr),
            AST::Or(_, lhs, rhs) => write!(f, "({} or {})", lhs, rhs),
            AST::Plus(_, lhs, rhs) => write!(f, "({} + {})", lhs, rhs),
            AST::Return(_, expr) => write!(f, "return {}", expr),
            AST::Slice {
                lhs,
                start,
                end,
                step,
                ..
            } => {
                write!(f, "{}[", lhs)?;
                if let Some(start) = start {
                    write!(f, "{}", start)?;
                }
                write!(f, ":")?;
                if let Some(end) = end {
                    write!(f, "{}", end)?;
                }
                if let Some(step) = step {
                    write!(f, ":{}", step)?;
                }
                write!(f, "]")
            }
            AST::StringLiteral(_, val) => write!(f, "\"{}\"", val),
            AST::VarDeclaration(_, name, expr) => write!(f, "let {} = {}", name, expr),
            AST::Variable(_, name) => write!(f, "{}", name),
            AST::Equals(_, lhs, rhs) => write!(f, "({} == {})", lhs, rhs),
            AST::NotEquals(_, lhs, rhs) => write!(f, "({} != {})", lhs, rhs),
            AST::LessThan(_, lhs, rhs) => write!(f, "({} < {})", lhs, rhs),
            AST::GreaterThan(_, lhs, rhs) => write!(f, "({} > {})", lhs, rhs),
            AST::LessEquals(_, lhs, rhs) => write!(f, "({} <= {})", lhs, rhs),
            AST::GreaterEquals(_, lhs, rhs) => write!(f, "({} >= {})", lhs, rhs),
            AST::While(_, cond, ..) => write!(f, "while {}", cond),
            AST::Continue(_) => write!(f, "continue"),
            AST::Break(_) => write!(f, "break"),
            AST::ForEach(_, name, iter, ..) => write!(f, "for {} in {}", name, iter),
            AST::For {
                init, cond, step, ..
            } => {
                write!(f, "for (")?;
                if let Some(init) = init {
                    write!(f, "{}", init)?;
                }
                write!(f, "; ")?;
                if let Some(cond) = cond {
                    write!(f, "{}", cond)?;
                }
                write!(f, "; ")?;
                if let Some(step) = step {
                    write!(f, "{}", step)?;
                }
                write!(f, ")")
            }
            AST::FieldAccess(_, lhs, rhs) => write!(f, "{}.{}", lhs, rhs),
            AST::Comprehension(_, var, iter, expr, cond) => match cond {
                Some(cond) => write!(f, "[{} for {} in {} if {}]", expr, var, iter, cond),
                None => write!(f, "[{} for {} in {}]", expr, var, iter),
            },
            AST::FormatStringLiteral(_, strings, exprs) => {
                write!(f, "\"")?;
                for (i, string) in strings.iter().enumerate() {
                    write!(f, "{}", string)?;
                    if i < exprs.len() {
                        write!(f, "{{{}}}", exprs[i])?;
                    }
                }
                write!(f, "\"")
            }
            AST::Range(_, start, end) => write!(f, "{}..{}", start, end),
            AST::PostIncrement(_, expr, offset) => {
                write!(f, "{}{}", expr, if *offset == 1 { "++" } else { "--" })
            }
            AST::PreIncrement(_, expr, offset) => {
                write!(f, "{}{}", if *offset == 1 { "++" } else { "--" }, expr)
            }
            AST::ArrayLiteral(_, exprs) | AST::TupleLiteral(_, exprs) => {
                write!(f, "[")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                write!(f, "]")
            }
            AST::DictionaryLiteral(_, items) => {
                write!(f, "{{")?;
                for (i, (key, value)) in items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", key, value)?;
                }
                write!(f, "}}")
            }
            AST::Namespace {
                span: _,
                name,
                body,
            } => write!(f, "namespace {} {{ {} }}", name, body),
            AST::Import {
                span: _,
                path,
                alias,
            } => {
                write!(f, "import {}", path)?;
                if let Some(alias) = alias {
                    write!(f, " as {}", alias)?;
                }
                Ok(())
            }
            AST::FromImport {
                span: _,
                path,
                names,
            } => {
                write!(f, "from {} import ", path)?;
                for (i, (name, alias)) in names.iter().enumerate() {
                    if i == 0 {
                        write!(f, "{{")?;
                    }
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", name)?;
                    if let Some(alias) = alias {
                        write!(f, " as {}", alias)?;
                    }
                }
                if !names.is_empty() {
                    write!(f, "}}")?;
                }
                Ok(())
            }
            AST::StarExpression(_, expr) => write!(f, "*{}", expr),
            AST::StarStarExpression(_, expr) => write!(f, "**{}", expr),
        }
    }
}
