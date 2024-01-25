use super::{binary_op::BinaryOp, expr::Expr, ident::Ident, lit::Lit, unary_op::UnaryOp};

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Apply { fun: Expr, arg: Expr },
    Unary { op: UnaryOp, expr: Expr },
    Binary { op: BinaryOp, lhs: Expr, rhs: Expr },
    If { cond: Expr, then: Expr, else_: Expr },
    Let { name: Ident, expr: Expr, body: Expr },
    Lambda { param: Ident, expr: Expr },
    Unit,
}
