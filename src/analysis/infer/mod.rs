use self::{
    constraint::Constraint,
    context::Context,
    error::{InferResult, TypeError},
    meta::Meta,
    meta_context::MetaContext,
    r#type::Type,
    registry::Registry,
    scheme::Scheme,
    tir::*,
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
use itertools::Itertools;
use std::{collections::HashMap, vec};

mod constraint;
mod context;
pub mod error;
mod meta;
mod meta_context;
pub mod registry;
mod scheme;
pub mod tir;
pub mod r#type;

#[derive(Debug, Clone)]
pub struct TypeSolver {
    src: InternedString,
    ctx: Context,
    meta_ctx: MetaContext,
    reg: Registry,
    builtins: HashMap<UniqueId, InternedString>,
    constraints: Vec<Constraint>,
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
            meta_ctx: MetaContext::new(),
            reg: Registry::new(),
            builtins,
            constraints: vec![],
            scoped_interner,
        }
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
                    decls.push(decl);
                }
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
            nir::DeclKind::Let(pat, let_expr) => {
                log::debug!("infer let ({:?})", pat);
                // to show that Γ ⊢ let x = e0 in e1 : T' we need to show that
                // Γ ⊢ e0 : T
                let mut solved_expr = self.infer_expr(let_expr)?;
                // log::debug!("let_solved_expr: {:?}", solved_expr);

                // Γ, x: gen(T) ⊢ e1 : T'
                let mut solved_pat = self.infer_pattern(pat, &solved_expr.ty, true)?;
                solved_pat.ty.unify(&mut solved_expr.ty)?;

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

                let mut param_vars = params.iter().map(|_| Meta::fresh()).collect_vec();
                let mut param_tys = param_vars.iter().map(|var| Type::Var(*var)).collect_vec();

                let mut ty_ret = Type::Var(Meta::fresh());
                let mut fn_ty = Type::Lambda(param_tys.clone(), Box::new(ty_ret.clone()));
                log::debug!("decl_fn_ty: {:?}", fn_ty);

                self.ctx
                    .insert(name.id, Scheme::new(param_vars.clone(), fn_ty.clone()));
                self.ctx.push();
                let mut solved_params = vec![];
                for (mut ty, param) in param_tys.iter().zip(params.iter()) {
                    let mut solved_pat = self.infer_pattern(param, ty, false)?;
                    solved_params.push(solved_pat.clone());
                    solved_pat.ty.unify(&mut ty)?;
                }
                let mut solved_expr = self.infer_expr(fn_expr)?;
                solved_expr.ty.unify(&mut ty_ret)?;
                // log::debug!("decl_fn_solved_expr: {:?}", solved_expr);
                self.ctx.pop();

                Ok(Decl {
                    kind: DeclKind::Fn(*name, solved_params, solved_expr.clone()),
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
                log::debug!("infer lambda");
                // to show that Γ ⊢ λx.e : T -> T' we need to show that
                // T = newvar
                // let param_ty = Type::Var(TyVar::fresh());

                // Γ, x : T ⊢ e : T'
                let param_types = vec![Type::Var(Meta::fresh()); params.len()];
                log::debug!("lambda_param_types: {:?}", param_types);
                self.ctx.push();
                let mut solved_params = vec![];
                for (param, mut ty) in params.iter().zip(param_types.iter()) {
                    let mut solved_pat = self.infer_pattern(param, ty, false)?;
                    solved_params.push(solved_pat.clone());
                    solved_pat.ty.unify(&mut ty)?;
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
                let mut solved_fun = self.infer_expr(fun)?;
                // log::debug!("app_solved_fun: {:?}", solved_fun);

                // Γ ⊢ e1 : T1
                // let solved_arg = self.infer_expr(arg)?;
                let solved_args = args
                    .iter()
                    .map(|arg| self.infer_expr(arg))
                    .collect::<InferResult<Vec<Expr>>>()?;
                // log::debug!("app_solved_args: {:?}", solved_args);

                // T' = newvar
                let ty_ret = Type::Var(Meta::fresh());
                log::debug!("app_ty_ret: {:?}", ty_ret);

                // unify(T0, T1 -> T')
                let ty_args = solved_args.iter().map(|arg| arg.ty).collect_vec();
                log::debug!("app_ty_args: {:?}", ty_args);
                solved_fun
                    .ty
                    .unify(&mut Type::Lambda(ty_args, Box::new(ty_ret)))?;

                Ok(Expr::new(
                    ExprKind::Apply(solved_fun, solved_args),
                    ty_ret,
                    expr.span,
                ))
            }
            nir::ExprKind::Or(lhs, rhs) => {
                let mut solved_lhs = self.infer_expr(lhs)?;
                let mut solved_rhs = self.infer_expr(rhs)?;

                solved_lhs.ty.unify(&mut Type::Bool)?;
                solved_rhs.ty.unify(&mut Type::Bool)?;

                Ok(Expr::new(
                    ExprKind::Or(solved_lhs, solved_rhs),
                    Type::Bool,
                    expr.span,
                ))
            }
            nir::ExprKind::And(lhs, rhs) => {
                let mut solved_lhs = self.infer_expr(lhs)?;
                let mut solved_rhs = self.infer_expr(rhs)?;

                solved_lhs.ty.unify(&mut Type::Bool)?;
                solved_rhs.ty.unify(&mut Type::Bool)?;

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
                let mut solved_expr = self.infer_expr(let_expr)?;
                // log::debug!("let_solved_expr: {:?}", solved_expr);

                // Γ, x: T ⊢ e1 : T'
                let mut solved_pat = self.infer_pattern(pat, &solved_expr.ty, false)?;
                solved_pat.ty.unify(&mut solved_expr.ty)?;

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

                let param_tys = vec![Type::Var(Meta::fresh()); params.len()];
                let mut ty_ret = Type::Var(Meta::fresh());
                let fn_ty = Type::Lambda(param_tys.clone(), Box::new(ty_ret.clone()));
                log::debug!("fn_ty: {:?}", fn_ty);

                self.ctx.push();
                self.ctx.insert(name.id, Scheme::new(vec![], fn_ty.clone()));
                self.ctx.push();
                let mut solved_params = vec![];
                for (mut ty, pat) in param_tys.iter().zip(params.iter()) {
                    let mut solved_pat = self.infer_pattern(pat, ty, false)?;
                    solved_params.push(solved_pat.clone());
                    solved_pat.ty.unify(&mut ty)?;
                }

                let mut solved_expr = self.infer_expr(expr)?;
                log::debug!("fn_solved_expr: {:?}", solved_expr.ty);
                solved_expr.ty.unify(&mut ty_ret)?;
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
                let mut solved_cond = self.infer_expr(cond)?;
                let mut solved_then = self.infer_expr(then)?;
                let mut solved_else = self.infer_expr(else_)?;

                solved_cond.ty.unify(&mut Type::Bool)?;
                solved_then.ty.unify(&mut solved_else.ty)?;

                Ok(Expr::new(
                    ExprKind::If(solved_cond, solved_then.clone(), solved_else),
                    solved_then.ty,
                    expr.span,
                ))
            }
            nir::ExprKind::Match(expr, arms) => {
                log::debug!("infer match");
                let mut solved_expr = self.infer_expr(expr)?;
                let mut ty = Type::Var(Meta::fresh());
                let mut solved_arms = vec![];
                for (pat, body) in arms {
                    self.ctx.push();
                    let mut solved_pat = self.infer_pattern(pat, &ty, false)?;
                    solved_pat.ty.unify(&mut solved_expr.ty)?;
                    let mut solved_body = self.infer_expr(body)?;
                    solved_body.ty.unify(&mut ty)?;
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
                let mut ty = Type::Var(Meta::fresh());

                let solved_exprs = exprs
                    .iter()
                    .map(|expr| self.infer_expr(expr))
                    .collect::<InferResult<Vec<Expr>>>()?;

                for mut expr in &solved_exprs {
                    // println!("expr: {:?}", expr.ty);
                    expr.ty.unify(&mut ty)?;
                }

                Ok(Expr::new(
                    ExprKind::List(solved_exprs),
                    Type::List(Box::new(ty)),
                    expr.span,
                ))
            }
            nir::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, Type::Unit, expr.span)),
        }
    }

    fn infer_pattern(
        &mut self,
        pat: &nir::Pattern,
        ty: &Type,
        generalize: bool,
    ) -> InferResult<Pattern> {
        match pat.kind.as_ref() {
            nir::PatternKind::Wildcard => Ok(Pattern::new(
                PatternKind::Wildcard,
                Type::Var(Meta::fresh()),
                pat.span,
            )),
            nir::PatternKind::Ident(name, hint) => {
                log::debug!("infer pattern ident: {:?}", name.id);
                if generalize {
                    self.ctx.insert(name.id, ty.generalize(&self.ctx));
                } else {
                    self.ctx.insert(name.id, Scheme::new(vec![], ty.clone()));
                }
                // self.ctx.insert(id, scheme)
                // let ty = Type::Var(TyVar::fresh());
                // log::debug!("infer_ident_ty: {:?}", ty);
                // self.ctx.insert(name.id, Scheme::new(vec![], ty.clone()));
                // if let Some(hint) = hint {
                //     let hint_ty = self.reg.get(hint).unwrap();
                //     ty.unify(hint_ty.clone())?);
                // }
                Ok(Pattern::new(
                    PatternKind::Ident(*name),
                    ty.clone(),
                    pat.span,
                ))
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
                let ty = Type::Var(Meta::fresh());
                let list_ty = Type::List(Box::new(ty.clone()));
                let solved_pats = pats
                    .iter()
                    .map(|pat| self.infer_pattern(pat, &ty, generalize))
                    .collect::<InferResult<Vec<Pattern>>>()?;

                Ok(Pattern::new(
                    PatternKind::List(solved_pats),
                    list_ty,
                    pat.span,
                ))
            }
            nir::PatternKind::Pair(lhs, rhs) => {
                let mut ty = Type::Var(Meta::fresh());
                let mut list_ty = Type::List(Box::new(ty.clone()));
                let mut solved_lhs = self.infer_pattern(lhs, &ty, generalize)?;
                let mut solved_rhs = self.infer_pattern(rhs, &list_ty, generalize)?;

                solved_lhs.ty.unify(&mut ty)?;
                solved_rhs.ty.unify(&mut list_ty)?;

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
