use std::fmt::Display;

use num_complex::Complex64;
use num_rational::Rational64;

use crate::{intern::InternedString, list::List};

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i64),
    Rational(Rational64),
    Real(f64),
    Complex(Complex64),
    String(InternedString),
    Char(char),
    Bool(bool),
    // List(List<Value>),
    // Tuple(Tuple),
    // Map(Map),
    // Record(Record),
    // Lambda(Lambda),
    Nil,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Rational(n) => write!(f, "{}", n),
            Value::Real(n) => write!(f, "{}", n),
            Value::Complex(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Char(c) => write!(f, "{}", c),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Nil => write!(f, "nil"),
        }
    }
}
