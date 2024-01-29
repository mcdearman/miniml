use super::{expr::Expr, pattern::Pattern};

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    RecordDef(RecordDef),
    Let { pattern: Pattern, expr: Expr },
}
