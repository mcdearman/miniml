use super::{data_type::DataType, expr::Expr, pattern::Pattern};

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    DataType(DataType),
    Let { pattern: Pattern, expr: Expr },
}
