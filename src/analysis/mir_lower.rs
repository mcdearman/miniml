use super::{
    error::{AnalysisError, AnalysisResult},
    infer::tir,
    mir::{self, *},
};

pub fn lower(root: tir::Root) -> (Option<Root>, Vec<AnalysisError>) {
    todo!()
}

fn lower_decl(decl: tir::Decl) -> AnalysisResult<Decl> {
    todo!()
}

fn lower_expr(expr: tir::Expr) -> AnalysisResult<Expr> {
    match expr.kind() {
        tir::ExprKind::Lit(lit) => match lit {
            tir::Lit::Int(i) => todo!(),
            tir::Lit::Rational(_) => todo!(),
            tir::Lit::Bool(_) => todo!(),
            tir::Lit::String(_) => todo!(),
        },
        tir::ExprKind::Ident(_) => todo!(),
        tir::ExprKind::Apply { fun, args } => todo!(),
        tir::ExprKind::Or { lhs, rhs } => todo!(),
        tir::ExprKind::And { lhs, rhs } => todo!(),
        tir::ExprKind::If { cond, then, else_ } => todo!(),
        tir::ExprKind::Let { name, expr, body } => todo!(),
        tir::ExprKind::Fn {
            name,
            params,
            expr,
            body,
        } => todo!(),
        tir::ExprKind::Lambda { params, expr } => todo!(),
        tir::ExprKind::List(_) => todo!(),
        tir::ExprKind::Record { name, fields } => todo!(),
        tir::ExprKind::Dot { expr, field } => todo!(),
        tir::ExprKind::Unit => todo!(),
    }
}
