use crate::{intern::InternedString, list::List};
use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(InternedString),
    Symbol(InternedString),
    List(List<Value>),
    Tuple(Vec<Value>),
    Map(HashMap<InternedString, Value>),
    Record(Record),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(value) => write!(f, "{}", value),
            Value::Float(value) => write!(f, "{}", value),
            Value::Bool(value) => write!(f, "{}", value),
            Value::String(value) => write!(f, "{}", value),
            Value::Symbol(value) => write!(f, "{}", value),
            Value::List(l) => write!(f, "{:?}", l.clone()),
            Value::Tuple(t) => write!(f, "{:?}", t),
            Value::Map(m) => write!(f, "{:?}", m),
            Value::Record(r) => write!(f, "{}", r),
            Value::Nil => write!(f, "nil"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub name: InternedString,
    pub fields: HashMap<InternedString, Value>,
}

impl Display for Record {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {{", self.name)?;
        for (i, (key, value)) in self.fields.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", key, value)?;
        }
        write!(f, "}}")
    }
}
