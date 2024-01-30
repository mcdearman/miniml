use miniml_utils::interned_string::InternedString;
use num_rational::Rational64;

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Num(Rational64),
    Bool(bool),
    String(InternedString),
}
