use crate::common::Span;
use std::rc::Rc;

#[derive(Debug)]
pub enum AST {
    And(Span, Rc<AST>, Rc<AST>),
    Assert(Span, Rc<AST>),
    Assignment(Span, Rc<AST>, Rc<AST>),
    Block(Span, Vec<Rc<AST>>),
    BooleanLiteral(Span, bool),
    Call(Span, Rc<AST>, Vec<Rc<AST>>),
    Divide(Span, Rc<AST>, Rc<AST>),
    FloatLiteral(Span, f64),
    Function {
        span: Span,
        name: Option<String>,
        args: Vec<String>,
        body: Rc<AST>,
    },
    If(Span, Rc<AST>, Rc<AST>, Option<Rc<AST>>),
    Index(Span, Rc<AST>, Rc<AST>),
    IntegerLiteral(Span, i64),
    Minus(Span, Rc<AST>, Rc<AST>),
    Multiply(Span, Rc<AST>, Rc<AST>),
    Not(Span, Rc<AST>),
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
    Range(Span, Rc<AST>, Rc<AST>),

    PostIncrement(Span, Rc<AST>, i64),
    PreIncrement(Span, Rc<AST>, i64),
    ArrayLiteral(Span, Vec<Rc<AST>>),
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
            AST::Divide(span, ..) => span,
            AST::FloatLiteral(span, ..) => span,
            AST::Function { span, .. } => span,
            AST::If(span, ..) => span,
            AST::Index(span, ..) => span,
            AST::IntegerLiteral(span, ..) => span,
            AST::Minus(span, ..) => span,
            AST::Multiply(span, ..) => span,
            AST::Not(span, ..) => span,
            AST::Nothing(span, ..) => span,
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
            AST::Range(span, ..) => span,
            AST::PostIncrement(span, ..) => span,
            AST::PreIncrement(span, ..) => span,
            AST::ArrayLiteral(span, ..) => span,
        }
    }
}

impl std::fmt::Display for AST {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            AST::And(_, lhs, rhs) => write!(f, "({} and {})", lhs, rhs),
            AST::Assert(_, expr) => write!(f, "assert {}", expr),
            AST::Assignment(_, lhs, rhs) => write!(f, "{} = {}", lhs, rhs),
            AST::Block(_, exprs) => write!(f, "<block with {} exprs>", exprs.len()),
            AST::BooleanLiteral(_, val) => write!(f, "{}", val),
            AST::Call(_, func, args) => {
                write!(f, "{}(", func)?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            AST::Divide(_, lhs, rhs) => write!(f, "({} / {})", lhs, rhs),
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
            AST::Not(_, expr) => write!(f, "not {}", expr),
            AST::Nothing(_) => write!(f, "nothing"),
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
            AST::For{init, cond, step, .. } => {
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
            },
            AST::Range(_, start, end) => write!(f, "{}..{}", start, end),
            AST::PostIncrement(_, expr, offset) => write!(f, "{}{}", expr, if *offset == 1 { "++" } else { "--" }),
            AST::PreIncrement(_, expr, offset) => write!(f, "{}{}", if *offset == 1 { "++" } else { "--" }, expr),
            AST::ArrayLiteral(_, exprs) => {
                write!(f, "[")?;
                for (i, expr) in exprs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", expr)?;
                }
                write!(f, "]")
            }
        }
    }
}
