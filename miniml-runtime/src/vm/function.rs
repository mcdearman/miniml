use crate::chunk::Chunk;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    name: String,
    arity: u8,
    chunk: Chunk,
}

impl Function {
    pub fn new(name: String, arity: u8, chunk: Chunk) -> Self {
        Self { name, arity, chunk }
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<Fn({})>", self.name)
    }
}
