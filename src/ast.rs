use crate::token::Location;

#[derive(Debug)]
pub enum AST {
    Call(Location, Box<AST>, Vec<Box<AST>>),
    Block(Location, Vec<Box<AST>>),
    Divide(Location, Box<AST>, Box<AST>),
    FloatLiteral(Location, f64),
    Index(Location, Box<AST>, Box<AST>),
    IntegerLiteral(Location, i64),
    Minus(Location, Box<AST>, Box<AST>),
    Multiply(Location, Box<AST>, Box<AST>),
    Plus(Location, Box<AST>, Box<AST>),
    Slice{loc: Location, lhs: Box<AST>, start:Option<Box<AST>>, end:Option<Box<AST>>, step:Option<Box<AST>>},
    StringLiteral(Location, String),
    VarDeclaration(Location, String, Box<AST>),
    Variable(Location, String),
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
        }
    }
}