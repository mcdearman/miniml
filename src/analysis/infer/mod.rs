use self::{
    context::Context,
    error::{InferResult, TypeError},
    r#type::Type,
    registry::Registry,
    scheme::Scheme,
    substitution::Substitution,
    tir::*,
    ty_var::TyVar,
};
use crate::{
    rename::nir,
    utils::{
        ident::ScopedIdent, intern::InternedString, list::List, scoped_intern::ScopedInterner,
        span::Span, unique_id::UniqueId,
    },
};
use std::collections::HashMap;

pub mod context;
pub mod error;
pub mod registry;
mod scheme;
pub mod substitution;
pub mod tir;
mod ty_var;
pub mod r#type;

#[derive(Debug, Clone)]
pub struct TypeSolver {
    src: InternedString,
    ctx: Context,
    reg: Registry,
    builtins: HashMap<UniqueId, InternedString>,
    sub: Substitution,
    scoped_interner: ScopedInterner,
}

impl TypeSolver {
    pub fn new(
        builtins: HashMap<UniqueId, InternedString>,
        scoped_interner: ScopedInterner,
    ) -> Self {
        Self {
            src: "".into(),
            ctx: Context::from_builtins(&builtins),
            reg: Registry::new(),
            builtins,
            sub: Substitution::new(),
            scoped_interner,
        }
    }

    pub fn infer<'src>(&mut self, src: &'src str, nir: &nir::Expr) -> InferResult<Expr> {
        self.src = src.into();
        self.infer_expr(nir).map(|e| e.apply_subst(&self.sub))
    }

    fn infer_expr(&mut self, expr: &nir::Expr) -> InferResult<Expr> {
        match expr.kind() {
            nir::ExprKind::Lit(lit) => match *lit {
                nir::Lit::Int(n) => Ok(Expr::new(
                    ExprKind::Lit(Lit::Int(n)),
                    Type::Int,
                    expr.span(),
                )),
                nir::Lit::Bool(b) => Ok(Expr::new(
                    ExprKind::Lit(Lit::Bool(b)),
                    Type::Bool,
                    expr.span(),
                )),
            },
            nir::ExprKind::Var(name) => {
                if let Some(scm) = self.ctx.get(&name.id()) {
                    let ty = scm.instantiate();
                    Ok(Expr::new(ExprKind::Var(*name), ty, expr.span()))
                } else {
                    Err(TypeError::from(format!(
                        "unbound variable: {:?} - \"{}\"",
                        expr,
                        self.src[name.span()].to_string()
                    )))
                }
            }
            nir::ExprKind::Abs(param, expr) => {
                // to show that Γ ⊢ λx.e : T -> T' we need to show that

                // T = newvar
                let param_ty = Type::Var(TyVar::fresh());

                // Γ, x : T ⊢ e : T'
                self.ctx
                    .insert(param.id(), Scheme::new(vec![], param_ty.clone()));
                let solved_expr = self.infer_expr(expr)?;
                let fun_ty = Type::Lambda(Box::new(param_ty), Box::new(solved_expr.ty()));

                Ok(Expr::new(
                    ExprKind::Abs(*param, solved_expr),
                    fun_ty,
                    expr.span(),
                ))
            }
            nir::ExprKind::App(fun, arg) => {
                // to show that Γ ⊢ e0 e1 : T' we need to show that
                // Γ ⊢ e0 : T0
                let solved_fun = self.infer_expr(fun)?;

                // Γ ⊢ e1 : T1
                let solved_arg = self.infer_expr(arg)?;

                // T' = newvar
                let ty_ret = Type::Var(TyVar::fresh());

                // unify(T0, T1 -> T')
                self.sub = solved_fun.ty().apply_subst(&self.sub).unify(&Type::Lambda(
                    Box::new(solved_arg.ty()),
                    Box::new(ty_ret.clone()),
                ))?;

                Ok(Expr::new(
                    ExprKind::App(solved_fun, solved_arg),
                    ty_ret,
                    expr.span(),
                ))
            }
            nir::ExprKind::Or(lhs, rhs) => {
                let solved_lhs = self.infer_expr(lhs)?;
                let solved_rhs = self.infer_expr(rhs)?;

                self.sub = solved_lhs.ty().unify(&Type::Bool)?;
                self.sub = solved_rhs.ty().unify(&Type::Bool)?;

                Ok(Expr::new(
                    ExprKind::Or(solved_lhs, solved_rhs),
                    Type::Bool,
                    expr.span(),
                ))
            }
            nir::ExprKind::And(lhs, rhs) => {
                let solved_lhs = self.infer_expr(lhs)?;
                let solved_rhs = self.infer_expr(rhs)?;

                self.sub = solved_lhs.ty().unify(&Type::Bool)?;
                self.sub = solved_rhs.ty().apply_subst(&self.sub).unify(&Type::Bool)?;

                Ok(Expr::new(
                    ExprKind::And(solved_lhs, solved_rhs),
                    Type::Bool,
                    expr.span(),
                ))
            }
            nir::ExprKind::Let(name, rec, expr, body) => {
                // to show that Γ ⊢ let x = e0 in e1 : T' we need to show that
                // Γ ⊢ e0 : T
                let solved_expr = self.infer_expr(expr)?;

                // Γ, x: gen(T) ⊢ e1 : T'
                let scheme = solved_expr.ty().generalize(&self.ctx);
                self.ctx.insert(name.id(), scheme);
                let solved_body = self.infer_expr(body)?;

                Ok(Expr::new(
                    ExprKind::Let(*name, *rec, solved_expr, solved_body.clone()),
                    solved_body.ty(),
                    expr.span(),
                ))
            }
            // nir::ExprKind::Fn {
            //     name,
            //     params,
            //     expr,
            //     body,
            // } => {
            //     let fun_ty_var = TyVar::fresh();
            //     let fun_ty = Type::Var(fun_ty_var.clone());
            //     self.ctx
            //         .insert(name.id(), Scheme::new(vec![], fun_ty.clone()));

            //     let mut param_types = vec![];

            //     for param in params {
            //         let param_type = Type::Var(TyVar::fresh());
            //         param_types.push(param_type.clone());
            //         self.ctx.insert(param.id(), Scheme::new(vec![], param_type));
            //     }

            //     let solved_expr = self.infer_expr(expr)?;
            //     let solved_body = self.infer_expr(body)?;

            //     // self.sub = fun_ty.unify(&Type::Lambda(param_types, Box::new(solved_expr.ty())))?;

            //     Ok(Expr::new(
            //         ExprKind::Fn {
            //             name: *name,
            //             params: params.clone(),
            //             expr: solved_expr,
            //             body: solved_body.clone(),
            //         },
            //         solved_body.ty(),
            //         expr.span(),
            //     ))
            // }
            // nir::ExprKind::If { cond, then, else_ } => {
            //     let solved_cond = self.infer_expr(cond)?;
            //     let solved_then = self.infer_expr(then)?;
            //     let solved_else_ = self.infer_expr(else_)?;

            //     self.sub = solved_cond.ty().unify(&Type::Bool)?;
            //     self.sub = solved_then.ty().unify(&solved_else_.ty())?;

            //     Ok(Expr::new(
            //         ExprKind::If {
            //             cond: solved_cond,
            //             then: solved_then.clone(),
            //             else_: solved_else_,
            //         },
            //         solved_then.ty(),
            //         expr.span(),
            //     ))
            // }
            nir::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, Type::Unit, expr.span())),
        }
    }

    // fn pretty_print_ctx(&self) {
    //     let mut ctx = HashMap::new();
    //     for (uid, scm) in self.ctx.iter() {
    //         let name = self.builtins.get(uid).map(|s| s.to_string());
    //         ctx.insert(name.unwrap_or_else(|| format!("{:?}", uid)), scm.clone());
    //     }

    //     println!("ctx: {:#?}", ctx);
    // }
}
