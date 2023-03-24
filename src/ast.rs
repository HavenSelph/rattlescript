use std::sync::Arc;

use crate::token::Location;

#[derive(Debug)]
pub enum AST {
    Call(Location, Arc<AST>, Vec<Arc<AST>>),
    Block(Location, Vec<Arc<AST>>),
    Divide(Location, Arc<AST>, Arc<AST>),
    FloatLiteral(Location, f64),
    Index(Location, Arc<AST>, Arc<AST>),
    IntegerLiteral(Location, i64),
    Minus(Location, Arc<AST>, Arc<AST>),
    Multiply(Location, Arc<AST>, Arc<AST>),
    Plus(Location, Arc<AST>, Arc<AST>),
    Slice{loc: Location, lhs: Arc<AST>, start:Option<Arc<AST>>, end:Option<Arc<AST>>, step:Option<Arc<AST>>},
    StringLiteral(Location, String),
    VarDeclaration(Location, String, Arc<AST>),
    Variable(Location, String),
    Function{loc: Location, name: String, args: Vec<String>, body: Arc<AST>},
    Return(Location, Arc<AST>),
    Assignment(Location, Arc<AST>, Arc<AST>),
}

impl AST {
    #[allow(dead_code)]
    pub fn location(&self) -> Location {
        match self {
            AST::Call(loc, _, _) => loc.clone(),
            AST::Block(loc, _) => loc.clone(),
            AST::Divide(loc, _, _) => loc.clone(),
            AST::FloatLiteral(loc, _) => loc.clone(),
            AST::Index(loc, _, _) => loc.clone(),
            AST::IntegerLiteral(loc, _) => loc.clone(),
            AST::Minus(loc, _, _) => loc.clone(),
            AST::Multiply(loc, _, _) => loc.clone(),
            AST::Plus(loc, _, _) => loc.clone(),
            AST::Slice{loc, ..} => loc.clone(),
            AST::StringLiteral(loc, _) => loc.clone(),
            AST::VarDeclaration(loc, _, _) => loc.clone(),
            AST::Variable(loc, _) => loc.clone(),
            AST::Function{loc, ..} => loc.clone(),
            AST::Return(loc, _) => loc.clone(),
            AST::Assignment(loc, _, _) => loc.clone(),
        }
    }
}
