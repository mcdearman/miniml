use super::r#type::Type;

#[derive(Debug, Clone, PartialEq)]
pub enum Constraint {
    Equal(Type, Type),
    Neg(Type, Type),
    Not(Type, Type),
    Add(Type, Type, Type),
    Sub(Type, Type, Type),
    Mul(Type, Type, Type),
    Div(Type, Type, Type),
    Rem(Type, Type, Type),
    Pow(Type, Type, Type),
    Eq(Type, Type, Type),
    Neq(Type, Type, Type),
    Lt(Type, Type, Type),
    Lte(Type, Type, Type),
    Gt(Type, Type, Type),
    Gte(Type, Type, Type),
    And(Type, Type, Type),
    Or(Type, Type, Type),
}
