use crate::parser::{Expr, Lit};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct CompilerError(pub String);

impl CompilerError {
    pub fn new(msg: &str) -> Self {
        Self(msg.to_string())
    }
}

impl Display for CompilerError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type Result<T> = std::result::Result<T, CompilerError>;

pub fn compile_expr(ast: &Expr) -> Result<Vec<u8>> {
    match ast {
        Expr::Lit(lit) => compile_lit(lit),
        _ => todo!(),
    }
}

fn compile_lit(lit: &Lit) -> Result<Vec<u8>> {
    match lit {
        Lit::Int(i) => Ok(vec![0x01, *i]),
        // Lit::Float(f) => Ok(vec![0x02, *f]),
        // Lit::Bool(b) => Ok(vec![0x02, *b as u8]),
        // Lit::Char(c) => Ok(vec![0x03, *c as u8]),
        // Lit::String(s) => Ok(vec![0x04, s.len() as u8]),
        _ => todo!(),
    }
}
