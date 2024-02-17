use super::{env::Env, error::Result};
use crate::utils::{intern::InternedString, unique_id::UniqueId};
use num_rational::Rational64;
use std::{cell::RefCell, fmt::Display, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Lit(Lit),
    Lambda { param: Vec<UniqueId>, expr: Expr },
    NativeFn(fn(Vec<Value>) -> Result<Value>),
    Unit,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Lit(Lit::Num(num)) => write!(f, "{}", num),
            Value::Lit(Lit::Bool(b)) => write!(f, "{}", b),
            Value::Lit(Lit::String(s)) => write!(f, "{}", s),
            Value::Lambda { .. } => write!(f, "<lambda>"),
            Value::NativeFn { .. } => write!(f, "<native fn>"),
            Value::Unit => write!(f, "()"),
        }
    }
}

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
