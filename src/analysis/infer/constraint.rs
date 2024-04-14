use super::r#type::Type;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Constraint {
    Add(Type, Type, Type),
    IsIn(Type, Type),
}
