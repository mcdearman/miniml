use super::{
    binary_op::BinaryOp, expr::Expr, ident::Ident, lit::Lit, pattern::Pattern, unary_op::UnaryOp,
};

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Apply {
        fun: Expr,
        arg: Expr,
    },
    Unary {
        op: UnaryOp,
        expr: Expr,
    },
    Binary {
        op: BinaryOp,
        lhs: Expr,
        rhs: Expr,
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
