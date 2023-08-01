use std::fmt::Display;

use miniml_util::intern::InternedString;

use crate::chunk::Chunk;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Function(Function),
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Object::Function(fun) => write!(f, "{}", fun),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub arity: usize,
    pub chunk: Box<Chunk>,
    pub name: InternedString,
}

impl Function {
    pub fn new(arity: usize, chunk: Box<Chunk>, name: &str) -> Self {
        Self {
            arity,
            chunk,
            name: name.into(),
        }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<fn {}>", self.name)
    }
}
