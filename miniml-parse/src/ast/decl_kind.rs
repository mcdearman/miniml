use super::{expr::Expr, pattern::Pattern};

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Let { pattern: Pattern, expr: Expr },
}
