use std::sync::Arc;

use crate::token::Location;

#[derive(Debug)]
pub enum AST {
    And(Location, Arc<AST>, Arc<AST>),
    Assignment(Location, Arc<AST>, Arc<AST>),
    Block(Location, Vec<Arc<AST>>),
    BooleanLiteral(Location, bool),
    Call(Location, Arc<AST>, Vec<Arc<AST>>),
    Divide(Location, Arc<AST>, Arc<AST>),
    FloatLiteral(Location, f64),
    Function{loc: Location, name: Option<String>, args: Vec<String>, body: Arc<AST>},
    If(Location, Arc<AST>, Arc<AST>, Option<Arc<AST>>),
    Index(Location, Arc<AST>, Arc<AST>),
    IntegerLiteral(Location, i64),
    Minus(Location, Arc<AST>, Arc<AST>),
    Multiply(Location, Arc<AST>, Arc<AST>),
    Not(Location, Arc<AST>),
    Nothing(Location),
    Or(Location, Arc<AST>, Arc<AST>),
    Plus(Location, Arc<AST>, Arc<AST>),
    Return(Location, Arc<AST>),
    Slice{loc: Location, lhs: Arc<AST>, start:Option<Arc<AST>>, end:Option<Arc<AST>>, step:Option<Arc<AST>>},
    StringLiteral(Location, String),
    VarDeclaration(Location, String, Arc<AST>),
    Variable(Location, String),
    Equals(Location, Arc<AST>, Arc<AST>),
    NotEquals(Location, Arc<AST>, Arc<AST>),
    LessThan(Location, Arc<AST>, Arc<AST>),
    GreaterThan(Location, Arc<AST>, Arc<AST>),
    LessThanEquals(Location, Arc<AST>, Arc<AST>),
    GreaterThanEquals(Location, Arc<AST>, Arc<AST>),
}
