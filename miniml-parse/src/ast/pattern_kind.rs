use super::{ident::Ident, pattern::Pattern};
use num_rational::Rational64;

#[derive(Debug, Clone, PartialEq)]
pub enum PatternKind {
    Wildcard,
    Ident(Ident),
    Num(Rational64),
    Bool(bool),
    String(String),
    Tuple(Vec<Pattern>),
    List(Vec<Pattern>),
    Cons(Vec<Pattern>, Box<Pattern>),
}
