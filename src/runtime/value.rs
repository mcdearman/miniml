use super::{env::Env, error::RuntimeResult};
use crate::{
    infer::tir::Expr,
    utils::{intern::InternedString, unique_id::UniqueId},
};
use std::{cell::RefCell, fmt::Display, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Lit(Lit),
    Lambda {
        env: Rc<RefCell<Env>>,
        params: Vec<UniqueId>,
        expr: Expr,
    },
    NativeFn(fn(Vec<Value>) -> RuntimeResult<Value>),
    Unit,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Lit(Lit::Int(i)) => write!(f, "{}", i),
            Value::Lit(Lit::Bool(b)) => write!(f, "{}", b),
            Value::Lit(Lit::String(s)) => write!(f, "{}", s),
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
    String(InternedString),
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Bool(b) => write!(f, "{}", b),
            Lit::String(s) => write!(f, "{}", s),
        }
    }
}
