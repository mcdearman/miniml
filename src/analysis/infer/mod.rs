use insta::internals::AutoName;
use itertools::Itertools;

use self::{
    constraint::Constraint,
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
        ident::ScopedIdent,
        intern::InternedString,
        list::{self, List},
        scoped_intern::ScopedInterner,
        span::Span,
        unique_id::UniqueId,
    },
};
use std::{collections::HashMap, vec};

mod constraint;
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
    constraints: Vec<Constraint>,
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
            constraints: vec![],
            sub: Substitution::new(),
            scoped_interner,
        }
    }

    pub fn sub(&self) -> &Substitution {
        &self.sub
    }

    pub fn infer<'src>(
        &mut self,
        src: &'src str,
        nir: &nir::Root,
    ) -> (Option<Root>, Vec<TypeError>) {
        log::debug!("infer: {:?}", nir.span);
        self.src = src.into();
        let mut decls = vec![];
        let mut errors = vec![];
        for decl in &nir.decls {
            match self.infer_decl(decl) {
                Ok(decl) => {
                    decls.push(decl.apply_subst(&self.sub));
                    self.ctx = self.ctx.apply_subst(&self.sub);
                }
                Err(e) => errors.push(e),
            }
        }

        // solve constraints
        // for c in &self.constraints {
        //     log::debug!("constraint: {:?}", c);
        //     match c {
        //         Constraint::Add(lhs, rhs, ret) => {

        //             // if let Some(ty) = self.sub.get(var) {
        //             //     if !ty.is_numeric() {
        //             //         errors.push(TypeError::from(format!(
        //             //             "expected number type, found: {:?}",
        //             //             ty
        //             //         )));
        //             //     }
        //             // } else {
        //             //     match ty.unify(&Type::Int) {
        //             //         Ok(sub) => {
        //             //             self.sub = self.sub.compose(&sub);
        //             //         }
        //             //         Err(e) => {
        //             //             errors.push(e);
        //             //         }
        //             //     }
        //             // }
        //             // _ => {
        //             //     errors.push(TypeError::from(format!(
        //             //         "expected number type, found: {:?}",
        //             //         ty
        //             //     )));
        //             // }
        //         }
        //     }
        // }

        if errors.is_empty() {
            (
                Some(
                    Root {
                        decls,
                        span: nir.span,
                    }
                    .apply_subst(&self.sub),
                ),
                errors,
            )
        } else {
            (None, errors)
        }
    }

    fn infer_decl(&mut self, decl: &nir::Decl) -> InferResult<Decl> {
        match &decl.kind {
            nir::DeclKind::Let(pat, let_expr) => {
                log::debug!("infer let ({:?})", pat);
                // to show that Γ ⊢ let x = e0 in e1 : T' we need to show that
                // Γ ⊢ e0 : T
                let solved_expr = self.infer_expr(let_expr)?;
                // log::debug!("let_solved_expr: {:?}", solved_expr);

                // Γ, x: gen(T) ⊢ e1 : T'
                let scheme = solved_expr.ty.generalize(&self.ctx);
                let solved_pat = self.infer_pattern(pat)?;
                self.sub = self.sub.compose(
                    &solved_pat
                        .ty
                        .apply_subst(&self.sub)
                        .unify(&solved_expr.ty.apply_subst(&self.sub))?,
                );

                Ok(Decl {
                    kind: DeclKind::Let(solved_pat, solved_expr.clone()),
                    ty: solved_expr.ty,
                    span: decl.span,
                })
            }
            nir::DeclKind::Fn(name, params, fn_expr) => {
                log::debug!("infer fn decl ({:?})", name);
                // to show that Γ ⊢ fn x = e0 in e1 : T' we need to show that
                // Γ, x: gen(T) ⊢ e1 : T'

                let param_vars = params.iter().map(|_| TyVar::fresh()).collect_vec();
                let param_tys = param_vars.iter().map(|var| Type::Var(*var)).collect_vec();

                let ty_ret = Type::Var(TyVar::fresh());
                let fn_ty = Type::Lambda(param_tys.clone(), Box::new(ty_ret.clone()));
                log::debug!("decl_fn_ty: {:?}", fn_ty);

                self.ctx
                    .insert(name.id, Scheme::new(param_vars.clone(), fn_ty.clone()));
                self.ctx.push();
                let mut solved_params = vec![];
                for (ty, param) in param_tys.iter().zip(params.iter()) {
                    let solved_pat = self.infer_pattern(param)?;
                    solved_params.push(solved_pat.clone());
                    self.sub = self.sub.compose(
                        &solved_pat
                            .ty
                            .apply_subst(&self.sub)
                            .unify(&ty.apply_subst(&self.sub))?,
                    );
                }
                let solved_expr = self.infer_expr(fn_expr)?;
                self.sub = self.sub.compose(
                    &solved_expr
                        .ty
                        .apply_subst(&self.sub)
                        .unify(&ty_ret.apply_subst(&self.sub))?,
                );
                log::debug!("decl_fn_sub: {:?}", self.sub);
                // log::debug!("decl_fn_solved_expr: {:?}", solved_expr);
                self.ctx.pop();

                Ok(Decl {
                    kind: DeclKind::Fn(*name, solved_params, solved_expr.clone()),
                    ty: fn_ty.apply_subst(&self.sub),
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
                log::debug!("infer lambda");
                // to show that Γ ⊢ λx.e : T -> T' we need to show that
                // T = newvar
                // let param_ty = Type::Var(TyVar::fresh());

                // Γ, x : T ⊢ e : T'
                let param_types = vec![Type::Var(TyVar::fresh()); params.len()];
                log::debug!("lambda_param_types: {:?}", param_types);
                self.ctx.push();
                let mut solved_params = vec![];
                for (param, ty) in params.iter().zip(param_types.iter()) {
                    let solved_pat = self.infer_pattern(param)?;
                    solved_params.push(solved_pat.clone());
                    self.sub = self.sub.compose(&solved_pat.ty.unify(ty)?);
                }
                // log::debug!("abs_ctx: {:?}", self.ctx);
                let solved_expr = self.infer_expr(fn_expr)?;
                // log::debug!("lambda_solved_expr: {:?}", solved_expr);
                let fun_ty = Type::Lambda(param_types, Box::new(solved_expr.ty.clone()));
                log::debug!("lambda_fun_ty: {:?}", fun_ty);
                self.ctx.pop();

                Ok(Expr::new(
                    ExprKind::Lambda(solved_params, solved_expr),
                    fun_ty,
                    expr.span,
                ))
            }
            nir::ExprKind::Apply(fun, args) => {
                log::debug!("infer app");
                // to show that Γ ⊢ e0 e1 : T' we need to show that
                // Γ ⊢ e0 : T0
                let solved_fun = self.infer_expr(fun)?;
                // log::debug!("app_solved_fun: {:?}", solved_fun);

                // Γ ⊢ e1 : T1
                // let solved_arg = self.infer_expr(arg)?;
                let solved_args = args
                    .iter()
                    .map(|arg| self.infer_expr(arg))
                    .collect::<InferResult<Vec<Expr>>>()?;
                // log::debug!("app_solved_args: {:?}", solved_args);

                // T' = newvar
                let ty_ret = Type::Var(TyVar::fresh());
                log::debug!("app_ty_ret: {:?}", ty_ret);

                // unify(T0, T1 -> T')
                let ty_args = solved_args.iter().map(|arg| arg.ty.clone()).collect_vec();
                log::debug!("app_ty_args: {:?}", ty_args);
                self.sub = self
                    .sub
                    .compose(&solved_fun.ty.apply_subst(&self.sub).unify(
                        &Type::Lambda(ty_args, Box::new(ty_ret.clone())).apply_subst(&self.sub),
                    )?);
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

                self.sub = self
                    .sub
                    .compose(&solved_lhs.ty.apply_subst(&self.sub).unify(&Type::Bool)?);
                self.sub = self
                    .sub
                    .compose(&solved_rhs.ty.apply_subst(&self.sub).unify(&Type::Bool)?);

                Ok(Expr::new(
                    ExprKind::Or(solved_lhs, solved_rhs),
                    Type::Bool,
                    expr.span,
                ))
            }
            nir::ExprKind::And(lhs, rhs) => {
                let solved_lhs = self.infer_expr(lhs)?;
                let solved_rhs = self.infer_expr(rhs)?;

                self.sub = self
                    .sub
                    .compose(&solved_lhs.ty.apply_subst(&self.sub).unify(&Type::Bool)?);
                self.sub = self
                    .sub
                    .compose(&solved_rhs.ty.apply_subst(&self.sub).unify(&Type::Bool)?);

                Ok(Expr::new(
                    ExprKind::And(solved_lhs, solved_rhs),
                    Type::Bool,
                    expr.span,
                ))
            }
            nir::ExprKind::Let(pat, let_expr, body) => {
                log::debug!("infer let ({:?})", pat);
                // to show that Γ ⊢ let x = e0 in e1 : T' we need to show that
                // Γ ⊢ e0 : T
                let solved_expr = self.infer_expr(let_expr)?;
                // log::debug!("let_solved_expr: {:?}", solved_expr);

                // Γ, x: T ⊢ e1 : T'
                let solved_pat = self.infer_pattern(pat)?;
                self.sub = self.sub.compose(
                    &solved_pat
                        .ty
                        .apply_subst(&self.sub)
                        .unify(&solved_expr.ty.apply_subst(&self.sub))?,
                );

                let solved_body = self.infer_expr(body)?;
                // log::debug!("let_solved_body: {:?}", solved_body);

                Ok(Expr::new(
                    ExprKind::Let(solved_pat, solved_expr, solved_body.clone()),
                    solved_body.ty,
                    expr.span,
                ))
            }
            nir::ExprKind::Fn(name, params, expr, body) => {
                log::debug!("infer fn ({:?})", name);
                // to show that Γ ⊢ fn x = e0 in e1 : T' we need to show that
                // Γ, x: gen(T) ⊢ e1 : T'

                let param_tys = vec![Type::Var(TyVar::fresh()); params.len()];
                let ty_ret = Type::Var(TyVar::fresh());
                let fn_ty = Type::Lambda(param_tys.clone(), Box::new(ty_ret.clone()));
                log::debug!("fn_ty: {:?}", fn_ty);

                self.ctx.push();
                self.ctx.insert(name.id, Scheme::new(vec![], fn_ty.clone()));
                self.ctx.push();
                let mut solved_params = vec![];
                for (ty, pat) in param_tys.iter().zip(params.iter()) {
                    let solved_pat = self.infer_pattern(pat)?;
                    solved_params.push(solved_pat.clone());
                    self.sub = self.sub.compose(
                        &solved_pat
                            .ty
                            .apply_subst(&self.sub)
                            .unify(&ty.apply_subst(&self.sub))?,
                    );
                }

                let solved_expr = self.infer_expr(expr)?;
                log::debug!("fn_solved_expr: {:?}", solved_expr.ty);
                self.sub = self.sub.compose(
                    &solved_expr
                        .ty
                        .apply_subst(&self.sub)
                        .unify(&ty_ret.apply_subst(&self.sub))?,
                );
                self.ctx.pop();

                let solved_body = self.infer_expr(body)?;
                log::debug!("fn_solved_body: {:?}", solved_body.ty);
                self.ctx.pop();

                Ok(Expr::new(
                    ExprKind::Fn(*name, solved_params, solved_expr, solved_body.clone()),
                    solved_body.ty,
                    expr.span,
                ))
            }
            nir::ExprKind::If(cond, then, else_) => {
                log::debug!("infer if");
                let solved_cond = self.infer_expr(cond)?;
                let solved_then = self.infer_expr(then)?;
                let solved_else = self.infer_expr(else_)?;

                let sub = self
                    .sub
                    .compose(&solved_cond.ty.apply_subst(&self.sub).unify(&Type::Bool)?);
                self.sub = sub.compose(
                    &solved_then
                        .ty
                        .apply_subst(&self.sub)
                        .unify(&solved_else.ty.apply_subst(&self.sub))?,
                );

                Ok(Expr::new(
                    ExprKind::If(solved_cond, solved_then.clone(), solved_else),
                    solved_then.ty,
                    expr.span,
                ))
            }
            nir::ExprKind::Match(expr, arms) => {
                log::debug!("infer match");
                let solved_expr = self.infer_expr(expr)?;
                let ty = Type::Var(TyVar::fresh());
                let mut solved_arms = vec![];
                for (pat, body) in arms {
                    self.ctx.push();
                    let solved_pat = self.infer_pattern(pat)?;
                    self.sub = self.sub.compose(
                        &solved_pat
                            .ty
                            .apply_subst(&self.sub)
                            .unify(&solved_expr.ty.apply_subst(&self.sub))?,
                    );
                    let solved_body = self.infer_expr(body)?;
                    self.sub = self.sub.compose(
                        &solved_body
                            .ty
                            .apply_subst(&self.sub)
                            .unify(&ty.apply_subst(&self.sub))?,
                    );
                    solved_arms.push((solved_pat, solved_body));
                    self.ctx.pop();
                }

                Ok(Expr::new(
                    ExprKind::Match(solved_expr, solved_arms),
                    ty,
                    expr.span,
                ))
            }
            nir::ExprKind::List(exprs) => {
                let ty = Type::Var(TyVar::fresh());

                let solved_exprs = exprs
                    .iter()
                    .map(|expr| self.infer_expr(expr))
                    .collect::<InferResult<Vec<Expr>>>()?;

                for expr in &solved_exprs {
                    // println!("expr: {:?}", expr.ty);
                    self.sub = self.sub.compose(
                        &expr
                            .ty
                            .apply_subst(&self.sub)
                            .unify(&ty.apply_subst(&self.sub))?,
                    );
                }

                Ok(Expr::new(
                    ExprKind::List(solved_exprs),
                    Type::List(Box::new(ty.apply_subst(&self.sub))),
                    expr.span,
                ))
            }
            nir::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, Type::Unit, expr.span)),
        }
    }

    fn infer_pattern(&mut self, pat: &nir::Pattern) -> InferResult<Pattern> {
        match pat.kind.as_ref() {
            nir::PatternKind::Wildcard => Ok(Pattern::new(
                PatternKind::Wildcard,
                Type::Var(TyVar::fresh()),
                pat.span,
            )),
            nir::PatternKind::Ident(name, hint) => {
                log::debug!("infer pattern ident: {:?}", name.id);
                let ty = Type::Var(TyVar::fresh());
                log::debug!("infer_ident_ty: {:?}", ty);
                self.ctx.insert(name.id, Scheme::new(vec![], ty.clone()));
                // if let Some(hint) = hint {
                //     let hint_ty = self.reg.get(hint).unwrap();
                //     self.sub = self.sub.compose(&ty.unify(hint_ty.clone())?);
                // }
                Ok(Pattern::new(PatternKind::Ident(*name), ty, pat.span))
            }
            nir::PatternKind::Lit(lit) => match *lit {
                nir::Lit::Byte(b) => Ok(Pattern::new(
                    PatternKind::Lit(Lit::Byte(b)),
                    Type::Byte,
                    pat.span,
                )),
                nir::Lit::Int(n) => Ok(Pattern::new(
                    PatternKind::Lit(Lit::Int(n)),
                    Type::Int,
                    pat.span,
                )),
                nir::Lit::Rational(r) => Ok(Pattern::new(
                    PatternKind::Lit(Lit::Rational(r)),
                    Type::Rational,
                    pat.span,
                )),
                nir::Lit::Real(r) => Ok(Pattern::new(
                    PatternKind::Lit(Lit::Real(r)),
                    Type::Real,
                    pat.span,
                )),
                nir::Lit::Bool(b) => Ok(Pattern::new(
                    PatternKind::Lit(Lit::Bool(b)),
                    Type::Bool,
                    pat.span,
                )),
                nir::Lit::String(s) => Ok(Pattern::new(
                    PatternKind::Lit(Lit::String(s.clone())),
                    Type::String,
                    pat.span,
                )),
                nir::Lit::Char(c) => Ok(Pattern::new(
                    PatternKind::Lit(Lit::Char(c)),
                    Type::Char,
                    pat.span,
                )),
            },
            nir::PatternKind::List(pats) => {
                let ty = Type::Var(TyVar::fresh());
                let solved_pats = pats
                    .iter()
                    .map(|pat| self.infer_pattern(pat))
                    .collect::<InferResult<Vec<Pattern>>>()?;

                Ok(Pattern::new(
                    PatternKind::List(solved_pats),
                    Type::List(Box::new(ty.apply_subst(&self.sub))),
                    pat.span,
                ))
            }
            nir::PatternKind::Pair(lhs, rhs) => {
                let ty = Type::Var(TyVar::fresh());
                let list_ty = Type::List(Box::new(ty.clone()));
                let solved_lhs = self.infer_pattern(lhs)?;
                let solved_rhs = self.infer_pattern(rhs)?;

                self.sub = self.sub.compose(
                    &solved_lhs
                        .ty
                        .apply_subst(&self.sub)
                        .unify(&ty.apply_subst(&self.sub))?,
                );
                self.sub = self.sub.compose(
                    &solved_rhs
                        .ty
                        .apply_subst(&self.sub)
                        .unify(&list_ty.apply_subst(&self.sub))?,
                );

                Ok(Pattern::new(
                    PatternKind::Pair(solved_lhs, solved_rhs),
                    list_ty,
                    pat.span,
                ))
            }
            nir::PatternKind::Unit => Ok(Pattern::new(PatternKind::Unit, Type::Unit, pat.span)),
        }
    }
}
