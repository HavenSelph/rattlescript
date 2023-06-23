/*
    Copyright (C) 2023  Haven Selph
    Check the LICENSE file for more information.
 */

use std::cell::RefCell;
use std::rc::Rc;

pub type Ref<T> = Rc<RefCell<T>>;

// macro_rules! get {
//     ($val:expr) => {
//         &*$val.borrow()
//     };
// }
// pub(crate) use get;

macro_rules! make {
    ($val:expr) => {
        std::rc::Rc::new(std::cell::RefCell::new($val))
    };
}
pub(crate) use make;

#[derive(Clone, Copy)]
pub struct Location {
    pub line: usize,
    pub column: usize,
    pub filename: &'static str,
}

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}:{}:{}", self.filename, self.line, self.column)
    }
}

impl std::fmt::Debug for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self)
    }
}

#[derive(Clone, Copy)]
pub struct Span(pub Location, pub Location);

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{} - {}", self.0, self.1)
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}", self)
    }
}

impl Span {
    pub fn extend(&self, other: &Span) -> Span {
        Span(self.0, other.1)
    }
}
