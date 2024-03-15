use super::{r#type::Type, scheme::Scheme};

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Eq(Type, Type),
    Inst(Scheme),
    Gen(Type),
}
