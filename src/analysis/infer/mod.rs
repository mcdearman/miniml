use insta::internals::AutoName;
use itertools::Itertools;

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
use std::{collections::HashMap, vec};

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

    pub fn infer<'src>(
        &mut self,
        src: &'src str,
        nir: &nir::Root,
    ) -> (Option<Root>, Vec<TypeError>) {
        self.src = src.into();
        println!("src: {:?}", self.src);
        let mut decls = vec![];
        let mut errors = vec![];
        for decl in &nir.decls {
            match self.infer_decl(decl) {
                Ok(decl) => decls.push(decl.apply_subst(&self.sub)),
                Err(e) => errors.push(e),
            }
        }
        if errors.is_empty() {
            (
                Some(Root {
                    decls,
                    span: nir.span,
                }),
                errors,
            )
        } else {
            (None, errors)
        }
    }

    fn infer_decl(&mut self, decl: &nir::Decl) -> InferResult<Decl> {
        match &decl.kind {
            nir::DeclKind::Let(name, let_expr) => {
                log::debug!("infer let ({:?}) = {:#?}", name, let_expr);
                // to show that Γ ⊢ let x = e0 in e1 : T' we need to show that
                // Γ ⊢ e0 : T
                let solved_expr = self.infer_expr(let_expr)?;
                log::debug!("let_solved_expr: {:?}", solved_expr);

                // Γ, x: gen(T) ⊢ e1 : T'
                let scheme = solved_expr.ty.generalize(&self.ctx);
                log::debug!("let_scheme: {:?}", scheme);
                self.ctx.insert(name.id, scheme);

                Ok(Decl {
                    kind: DeclKind::Let(*name, solved_expr.clone()),
                    ty: solved_expr.ty,
                    span: decl.span,
                })
            }
            nir::DeclKind::Fn(name, params, fn_expr) => {
                log::debug!("infer fn decl ({:?}) = {:#?}", name, fn_expr);
                // to show that Γ ⊢ fn x = e0 in e1 : T' we need to show that
                // Γ, x: gen(T) ⊢ e1 : T'
                let param_tys = params
                    .iter()
                    .map(|_| Type::Var(TyVar::fresh()))
                    .collect_vec();
                let ty_ret = Type::Var(TyVar::fresh());
                let fn_ty = Type::Lambda(param_tys.clone(), Box::new(ty_ret.clone()));
                log::debug!("decl_fn_ty: {:?}", fn_ty);

                self.ctx.push();
                self.ctx.insert(name.id, Scheme::new(vec![], fn_ty.clone()));
                params.iter().zip(param_tys.iter()).for_each(|(param, ty)| {
                    self.ctx.insert(param.id, Scheme::new(vec![], ty.clone()));
                });
                let solved_expr = self.infer_expr(fn_expr)?;
                self.sub = self.sub.compose(
                    &solved_expr
                        .ty
                        .apply_subst(&self.sub)
                        .unify(&ty_ret.apply_subst(&self.sub))?,
                );
                log::debug!("decl_fn_sub: {:?}", self.sub);
                log::debug!("decl_fn_solved_expr: {:?}", solved_expr);
                self.ctx.pop();

                Ok(Decl {
                    kind: DeclKind::Fn(*name, params.clone(), solved_expr.clone()),
                    ty: fn_ty,
                    span: decl.span,
                })
            }
        }
    }

    fn infer_expr(&mut self, expr: &nir::Expr) -> InferResult<Expr> {
        match expr.kind.as_ref() {
            nir::ExprKind::Lit(lit) => match *lit {
                nir::Lit::Byte(b) => Ok(Expr::new(
                    ExprKind::Lit(Lit::Byte(b)),
                    Type::Byte,
                    expr.span,
                )),
                nir::Lit::Int(n) => Ok(Expr::new(ExprKind::Lit(Lit::Int(n)), Type::Int, expr.span)),
                nir::Lit::Rational(r) => Ok(Expr::new(
                    ExprKind::Lit(Lit::Rational(r)),
                    Type::Rational,
                    expr.span,
                )),
                nir::Lit::Real(r) => Ok(Expr::new(
                    ExprKind::Lit(Lit::Real(r)),
                    Type::Real,
                    expr.span,
                )),
                nir::Lit::Bool(b) => Ok(Expr::new(
                    ExprKind::Lit(Lit::Bool(b)),
                    Type::Bool,
                    expr.span,
                )),
                nir::Lit::String(s) => Ok(Expr::new(
                    ExprKind::Lit(Lit::String(s.clone())),
                    Type::String,
                    expr.span,
                )),
                nir::Lit::Char(c) => Ok(Expr::new(
                    ExprKind::Lit(Lit::Char(c)),
                    Type::Char,
                    expr.span,
                )),
            },
            nir::ExprKind::Var(name) => {
                // to show that Γ ⊢ x : T we need to show that

                // x : σ ∈ Γ
                log::debug!("infer var: {:?}", name.id);
                if let Some(scm) = self.ctx.get(&name.id) {
                    // T = inst(σ)
                    log::debug!("var_scm: {:?}", scm);
                    let ty = scm.instantiate();
                    log::debug!("var_ty: {:?}", ty);

                    Ok(Expr::new(ExprKind::Var(*name), ty, expr.span))
                } else {
                    Err(TypeError::from(format!(
                        "unbound variable: {:?} - \"{}\"",
                        expr,
                        self.src[name.span].to_string()
                    )))
                }
            }
            nir::ExprKind::Lambda(params, fn_expr) => {
                log::debug!("infer lambda: {:?} and {:?}", params, fn_expr);
                // to show that Γ ⊢ λx.e : T -> T' we need to show that
                // T = newvar
                // let param_ty = Type::Var(TyVar::fresh());

                // Γ, x : T ⊢ e : T'
                let param_types = vec![Type::Var(TyVar::fresh()); params.len()];
                log::debug!("lambda_param_types: {:?}", param_types);
                self.ctx.push();
                for (param, ty) in params.iter().zip(param_types.iter()) {
                    self.ctx.insert(param.id, Scheme::new(vec![], ty.clone()));
                }
                // log::debug!("abs_ctx: {:?}", self.ctx);
                let solved_expr = self.infer_expr(fn_expr)?;
                log::debug!("lambda_solved_expr: {:?}", solved_expr);
                let fun_ty = Type::Lambda(param_types, Box::new(solved_expr.ty.clone()));
                log::debug!("lambda_fun_ty: {:?}", fun_ty);
                self.ctx.pop();

                Ok(Expr::new(
                    ExprKind::Lambda(params.clone(), solved_expr),
                    fun_ty,
                    expr.span,
                ))
            }
            nir::ExprKind::Apply(fun, args) => {
                log::debug!("infer app: {:?} and {:?}", fun, args);
                // to show that Γ ⊢ e0 e1 : T' we need to show that
                // Γ ⊢ e0 : T0
                let solved_fun = self.infer_expr(fun)?;
                log::debug!("app_solved_fun: {:?}", solved_fun);

                // Γ ⊢ e1 : T1
                // let solved_arg = self.infer_expr(arg)?;
                let solved_args = args
                    .iter()
                    .map(|arg| self.infer_expr(arg))
                    .collect::<InferResult<Vec<Expr>>>()?;
                log::debug!("app_solved_args: {:?}", solved_args);

                // T' = newvar
                let ty_ret = Type::Var(TyVar::fresh());
                log::debug!("app_ty_ret: {:?}", ty_ret);

                // unify(T0, T1 -> T')
                let ty_args = solved_args.iter().map(|arg| arg.ty.clone()).collect_vec();
                log::debug!("app_ty_args: {:?}", ty_args);
                self.sub = solved_fun.ty.unify(&Type::Lambda(
                    // solved_args.iter().map(|arg| arg.ty.clone()).collect(),
                    ty_args,
                    Box::new(ty_ret.clone()),
                ))?;
                log::debug!("app_sub: {:?}", self.sub);

                Ok(Expr::new(
                    ExprKind::Apply(solved_fun, solved_args),
                    ty_ret,
                    expr.span,
                ))
            }
            nir::ExprKind::Or(lhs, rhs) => {
                let solved_lhs = self.infer_expr(lhs)?;
                let solved_rhs = self.infer_expr(rhs)?;

                self.sub = solved_lhs.ty.unify(&Type::Bool)?;
                self.sub = solved_rhs.ty.unify(&Type::Bool)?;

                Ok(Expr::new(
                    ExprKind::Or(solved_lhs, solved_rhs),
                    Type::Bool,
                    expr.span,
                ))
            }
            nir::ExprKind::And(lhs, rhs) => {
                let solved_lhs = self.infer_expr(lhs)?;
                let solved_rhs = self.infer_expr(rhs)?;

                self.sub = solved_lhs.ty.unify(&Type::Bool)?;
                self.sub = solved_rhs.ty.apply_subst(&self.sub).unify(&Type::Bool)?;

                Ok(Expr::new(
                    ExprKind::And(solved_lhs, solved_rhs),
                    Type::Bool,
                    expr.span,
                ))
            }
            nir::ExprKind::Let(name, let_expr, body) => {
                log::debug!("infer let ({:?}) = {:#?} in {:#?}", name, let_expr, body);
                // to show that Γ ⊢ let x = e0 in e1 : T' we need to show that
                // Γ ⊢ e0 : T
                let solved_expr = self.infer_expr(let_expr)?;
                log::debug!("let_solved_expr: {:?}", solved_expr);

                // Γ, x: T ⊢ e1 : T'
                let scheme = Scheme::new(vec![], solved_expr.ty.clone());
                log::debug!("let_scheme: {:?}", scheme);
                self.ctx.insert(name.id, scheme);
                let solved_body = self.infer_expr(body)?;
                log::debug!("let_solved_body: {:?}", solved_body);

                Ok(Expr::new(
                    ExprKind::Let(*name, solved_expr, solved_body.clone()),
                    solved_body.ty,
                    expr.span,
                ))
            }
            nir::ExprKind::Fn(name, params, expr, body) => {
                log::debug!("infer fn ({:?}) = {:#?}", name, expr);
                // to show that Γ ⊢ fn x = e0 in e1 : T' we need to show that
                // Γ, x: gen(T) ⊢ e1 : T'
                let param_tys = params
                    .iter()
                    .map(|_| Type::Var(TyVar::fresh()))
                    .collect_vec();
                let fn_ty = Type::Lambda(param_tys.clone(), Box::new(Type::Var(TyVar::fresh())));
                log::debug!("fn_ty: {:?}", fn_ty);

                self.ctx.push();
                self.ctx.insert(name.id, Scheme::new(vec![], fn_ty.clone()));
                let solved_expr = self.infer_expr(expr)?;
                log::debug!("fn_solved_expr: {:?}", solved_expr);

                let solved_body = self.infer_expr(body)?;
                log::debug!("fn_solved_body: {:?}", solved_body);
                self.ctx.pop();

                Ok(Expr::new(
                    ExprKind::Fn(*name, params.clone(), solved_expr, solved_body),
                    fn_ty,
                    expr.span,
                ))
            }
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
            nir::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, Type::Unit, expr.span)),
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
