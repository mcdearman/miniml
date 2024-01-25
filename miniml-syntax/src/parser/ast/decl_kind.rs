use super::{expr::Expr, ident::Ident};

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Let { name: Ident, expr: Expr },
}
