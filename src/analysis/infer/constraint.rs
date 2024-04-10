use super::r#type::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Num(Type),
}
