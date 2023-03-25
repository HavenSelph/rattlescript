use crate::token::Location;
use std::rc::Rc;

#[derive(Debug)]
pub enum AST {
    And(Location, Rc<AST>, Rc<AST>),
    Assert(Location, Rc<AST>),
    Assignment(Location, Rc<AST>, Rc<AST>),
    Block(Location, Vec<Rc<AST>>),
    BooleanLiteral(Location, bool),
    Call(Location, Rc<AST>, Vec<Rc<AST>>),
    Divide(Location, Rc<AST>, Rc<AST>),
    FloatLiteral(Location, f64),
    Function {
        loc: Location,
        name: Option<String>,
        args: Vec<String>,
        body: Rc<AST>,
    },
    If(Location, Rc<AST>, Rc<AST>, Option<Rc<AST>>),
    Index(Location, Rc<AST>, Rc<AST>),
    IntegerLiteral(Location, i64),
    Minus(Location, Rc<AST>, Rc<AST>),
    Multiply(Location, Rc<AST>, Rc<AST>),
    Not(Location, Rc<AST>),
    Nothing(Location),
    Or(Location, Rc<AST>, Rc<AST>),
    Plus(Location, Rc<AST>, Rc<AST>),
    Return(Location, Rc<AST>),
    Slice {
        loc: Location,
        lhs: Rc<AST>,
        start: Option<Rc<AST>>,
        end: Option<Rc<AST>>,
        step: Option<Rc<AST>>,
    },
    StringLiteral(Location, String),
    VarDeclaration(Location, String, Rc<AST>),
    Variable(Location, String),
    Equals(Location, Rc<AST>, Rc<AST>),
    NotEquals(Location, Rc<AST>, Rc<AST>),
    LessThan(Location, Rc<AST>, Rc<AST>),
    GreaterThan(Location, Rc<AST>, Rc<AST>),
    LessThanEquals(Location, Rc<AST>, Rc<AST>),
    GreaterThanEquals(Location, Rc<AST>, Rc<AST>),
    While(Location, Rc<AST>, Rc<AST>),
    Continue(Location),
    Break(Location),
    For(Location, String, Rc<AST>, Rc<AST>),
    Range(Location, Rc<AST>, Rc<AST>),
}
