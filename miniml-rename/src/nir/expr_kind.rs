use super::{expr::Expr, ident::Ident, lit::Lit, pattern::Pattern};

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Apply {
        fun: Expr,
        arg: Expr,
    },
    If {
        cond: Expr,
        then: Expr,
        else_: Expr,
    },
    Let {
        pattern: Pattern,
        expr: Expr,
        body: Expr,
    },
    Lambda {
        param: Pattern,
        expr: Expr,
    },
    Unit,
}
