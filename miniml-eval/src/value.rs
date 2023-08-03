use itertools::join;
use miniml_util::{intern::InternedString, list::List};
use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Real(f64),
    String(String),
    List(List<Value>),
    Tuple(Vec<Value>),
    Record(HashMap<InternedString, Value>),
    Fn {
        params: Vec<InternedString>,
        body: Box<Value>,
    },
    Unit,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Real(r) => write!(f, "{}", r),
            Value::String(s) => write!(f, "{}", s),
            Value::List(l) => write!(f, "{}", l),
            Value::Tuple(t) => write!(f, "({})", join(t.clone(), ", ")),
            Value::Record(r) => write!(
                f,
                "{{{}}}",
                join(
                    r.clone().into_iter().map(|(k, v)| format!("{} = {}", k, v)),
                    ", "
                )
            ),
            Value::Fn { params, body } => write!(
                f,
                "fn {} => {}",
                join(params.clone().into_iter(), ", "),
                body
            ),
            Value::Unit => write!(f, "()"),
        }
    }
}
