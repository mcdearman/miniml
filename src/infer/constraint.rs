use super::r#type::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Eq(Type, Type),
}
