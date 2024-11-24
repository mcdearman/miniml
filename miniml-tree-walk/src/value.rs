use super::{env::Env, error::RuntimeResult};
use crate::{
    analysis::infer::tir::{self, Expr, Pattern},
    utils::{intern::InternedString, list::List, unique_id::UniqueId},
};
use itertools::Itertools;
use num_rational::Rational64;
use std::{cell::RefCell, fmt::Display, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Lit(Lit),
    Lambda(Rc<RefCell<Env>>, Pattern, Expr),
    NativeFn(NativeFn),
    Thunk(Rc<RefCell<Env>>, Expr),
    Tuple(Vec<Value>),
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
            Value::Thunk { .. } => write!(f, "<thunk>"),
            Value::Tuple(tuple) => write!(f, "({})", tuple.iter().format(", ")),
            Value::List(list) => write!(f, "{}", list),
            Value::Record(record) => write!(f, "{}", record),
            Value::Unit => write!(f, "()"),
        }
    }
}

impl Eq for Value {}

#[derive(Debug, Clone, PartialEq)]
pub struct NativeFn {
    pub args: Vec<Value>,
    pub arity: usize,
    pub f: fn(Vec<Value>) -> RuntimeResult<Value>,
}

impl NativeFn {
    pub fn call(&self, arg: Value) -> RuntimeResult<Value> {
        let mut args = self.args.clone();
        args.push(arg);
        log::trace!("NativeFn::call: args={:?}", args);
        if args.len() < self.arity {
            Ok(Value::NativeFn(NativeFn {
                args,
                arity: self.arity,
                f: self.f,
            }))
        } else if args.len() == self.arity {
            (self.f)(args)
        } else {
            Err(super::error::RuntimeError::ArityError(
                self.arity,
                args.len(),
            ))
        }
    }
}

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

impl PartialEq<Lit> for tir::Lit {
    fn eq(&self, other: &Lit) -> bool {
        match (self, other) {
            (tir::Lit::Byte(a), Lit::Byte(b)) => a == b,
            (tir::Lit::Int(a), Lit::Int(b)) => a == b,
            (tir::Lit::Rational(a), Lit::Rational(b)) => a == b,
            (tir::Lit::Real(a), Lit::Real(b)) => a == b,
            (tir::Lit::Bool(a), Lit::Bool(b)) => a == b,
            (tir::Lit::String(a), Lit::String(b)) => a == b,
            (tir::Lit::Char(a), Lit::Char(b)) => a == b,
            _ => false,
        }
    }
}
