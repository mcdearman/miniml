use super::error::RuntimeResult;
use crate::{infer::tir::Expr, utils::unique_id::UniqueId};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Lit(Lit),
    Lambda { params: Vec<UniqueId>, expr: Expr },
    NativeFn(fn(Vec<Value>) -> RuntimeResult<Value>),
    Unit,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Lit(Lit::Int(i)) => write!(f, "{}", i),
            Value::Lit(Lit::Bool(b)) => write!(f, "{}", b),
            Value::Lambda { .. } => write!(f, "<lambda>"),
            Value::NativeFn { .. } => write!(f, "<native fn>"),
            Value::Unit => write!(f, "()"),
        }
    }
}

impl Eq for Value {}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Bool(bool),
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Bool(b) => write!(f, "{}", b),
        }
    }
}
