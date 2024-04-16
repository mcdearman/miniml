use num_rational::Rational64;

use super::{env::Env, error::RuntimeResult};
use crate::{
    analysis::infer::tir::Expr,
    utils::{intern::InternedString, list::List, unique_id::UniqueId},
};
use std::{cell::RefCell, fmt::Display, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Lit(Lit),
    Lambda(Rc<RefCell<Env>>, Vec<UniqueId>, Expr),
    NativeFn(fn(Vec<Value>) -> RuntimeResult<Value>),
    List(List<Value>),
    Record(Record),
    Unit,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Lit(lit) => write!(f, "{}", lit),
            Value::Lambda { .. } => write!(f, "<lambda>"),
            Value::NativeFn { .. } => write!(f, "<native fn>"),
            Value::List(list) => write!(f, "{}", list),
            Value::Record(record) => write!(f, "{}", record),
            Value::Unit => write!(f, "()"),
        }
    }
}

impl Eq for Value {}

#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    name: UniqueId,
    fields: Vec<(InternedString, Value)>,
}

impl Record {
    pub fn new(name: UniqueId, fields: Vec<(InternedString, Value)>) -> Self {
        Self { name, fields }
    }

    pub fn name(&self) -> UniqueId {
        self.name
    }

    pub fn fields(&self) -> &[(InternedString, Value)] {
        &self.fields
    }

    pub fn get(&self, field: &str) -> Option<&Value> {
        self.fields
            .iter()
            .find_map(|(k, v)| if &**k == field { Some(v) } else { None })
    }
}

impl Display for Record {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, (k, v)) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", k, v)?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Byte(u8),
    Int(i64),
    Rational(Rational64),
    Real(f64),
    Bool(bool),
    String(InternedString),
    Char(char),
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Byte(b) => write!(f, "{}", b),
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Rational(r) => write!(f, "{}", r),
            Lit::Real(r) => write!(f, "{}", r),
            Lit::Bool(b) => write!(f, "{}", b),
            Lit::String(s) => write!(f, "{}", s),
            Lit::Char(c) => write!(f, "{}", c),
        }
    }
}
