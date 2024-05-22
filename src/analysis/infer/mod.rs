use self::{
    constraint::Constraint,
    context::Context,
    error::{InferResult, TypeError},
    meta::Meta,
    meta_context::MetaContext,
    r#type::Type,
    registry::Registry,
    scheme::PolyType,
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
        let mut meta_ctx = MetaContext::new();
        Self {
            src: "".into(),
            ctx: Context::from_builtins(&builtins, &mut meta_ctx),
            meta_ctx,
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
                let solved_expr = self.infer_expr(let_expr)?;

                // Γ, x: gen(T) ⊢ e1 : T'
                let solved_pat = self.infer_pattern(pat, &solved_expr.ty, true)?;
                self.unify(&solved_pat.ty, &solved_expr.ty)?;

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

                let param_vars = params.iter().map(|_| Meta::fresh()).collect_vec();
                let param_tys = param_vars
                    .iter()
                    .map(|var| Type::Meta(var.clone()))
                    .collect_vec();

                let ty_ret = Type::Meta(Meta::fresh());
                let fn_ty = Type::Lambda(param_tys.clone(), Box::new(ty_ret.clone()));
                log::debug!("decl_fn_ty: {:?}", fn_ty);

                self.ctx
                    .insert(name.id, PolyType::new(param_vars.clone(), fn_ty.clone()));
                self.ctx.push();

                let mut solved_params = vec![];
                for (ty, param) in param_tys.iter().zip(params.iter()) {
                    let solved_pat = self.infer_pattern(param, ty, false)?;
                    solved_params.push(solved_pat.clone());
                    self.unify(&solved_pat.ty, &ty)?;
                }
                let solved_expr = self.infer_expr(fn_expr)?;
                self.unify(&solved_expr.ty, &ty_ret)?;

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
                    let ty = scm.instantiate(&mut self.meta_ctx);
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

                // Γ, x : T ⊢ e : T'
                let param_types = vec![Type::Meta(Meta::fresh()); params.len()];
                log::debug!("lambda_param_types: {:?}", param_types);
                self.ctx.push();
                let mut solved_params = vec![];
                for (param, ty) in params.iter().zip(param_types.iter()) {
                    let solved_pat = self.infer_pattern(param, ty, false)?;
                    solved_params.push(solved_pat.clone());
                    self.unify(&solved_pat.ty, &ty)?;
                }
                let solved_expr = self.infer_expr(fn_expr)?;
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
                let ty_ret = Type::Meta(Meta::fresh());
                log::debug!("app_ty_ret: {:?}", ty_ret);

                // unify(T0, T1 -> T')
                let ty_args = solved_args.iter().map(|arg| arg.ty.clone()).collect_vec();
                log::debug!("app_ty_args: {:?}", ty_args);
                // solved_fun
                //     .ty
                //     .unify(&mut Type::Lambda(ty_args, Box::new(ty_ret)))?;
                self.unify(
                    &solved_fun.ty,
                    &Type::Lambda(ty_args.clone(), Box::new(ty_ret.clone())),
                )?;

                Ok(Expr::new(
                    ExprKind::Apply(solved_fun, solved_args),
                    ty_ret,
                    expr.span,
                ))
            }
            nir::ExprKind::Or(lhs, rhs) => {
                let solved_lhs = self.infer_expr(lhs)?;
                let solved_rhs = self.infer_expr(rhs)?;

                self.unify(&solved_lhs.ty, &Type::Bool)?;
                self.unify(&solved_rhs.ty, &Type::Bool)?;

                Ok(Expr::new(
                    ExprKind::Or(solved_lhs, solved_rhs),
                    Type::Bool,
                    expr.span,
                ))
            }
            nir::ExprKind::And(lhs, rhs) => {
                let solved_lhs = self.infer_expr(lhs)?;
                let solved_rhs = self.infer_expr(rhs)?;

                self.unify(&solved_lhs.ty, &Type::Bool)?;
                self.unify(&solved_rhs.ty, &Type::Bool)?;

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

                // Γ, x: T ⊢ e1 : T'
                let solved_pat = self.infer_pattern(pat, &solved_expr.ty, false)?;
                self.unify(&solved_pat.ty, &solved_expr.ty)?;

                let solved_body = self.infer_expr(body)?;

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

                let param_tys = vec![Type::Meta(Meta::fresh()); params.len()];
                let ty_ret = Type::Meta(Meta::fresh());
                let fn_ty = Type::Lambda(param_tys.clone(), Box::new(ty_ret.clone()));
                log::debug!("fn_ty: {:?}", fn_ty);

                self.ctx.push();
                self.ctx
                    .insert(name.id, PolyType::new(vec![], fn_ty.clone()));
                self.ctx.push();
                let mut solved_params = vec![];
                for (ty, pat) in param_tys.iter().zip(params.iter()) {
                    let solved_pat = self.infer_pattern(pat, ty, false)?;
                    solved_params.push(solved_pat.clone());
                    self.unify(&solved_pat.ty, &ty)?;
                }

                let solved_expr = self.infer_expr(expr)?;
                log::debug!("fn_solved_expr: {:?}", solved_expr.ty);
                self.unify(&solved_expr.ty, &ty_ret)?;
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

                self.unify(&solved_cond.ty, &Type::Bool)?;
                self.unify(&solved_then.ty, &solved_else.ty)?;

                Ok(Expr::new(
                    ExprKind::If(solved_cond, solved_then.clone(), solved_else),
                    solved_then.ty,
                    expr.span,
                ))
            }
            nir::ExprKind::Match(expr, arms) => {
                log::debug!("infer match");
                let solved_expr = self.infer_expr(expr)?;
                let ty = Type::Meta(Meta::fresh());
                let mut solved_arms = vec![];
                for (pat, body) in arms {
                    self.ctx.push();
                    let solved_pat = self.infer_pattern(pat, &ty, false)?;
                    // solved_pat.ty.unify(&mut solved_expr.ty)?;
                    self.unify(&solved_pat.ty, &solved_expr.ty)?;
                    let solved_body = self.infer_expr(body)?;
                    // solved_body.ty.unify(&mut ty)?;
                    self.unify(&solved_body.ty, &ty)?;
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
                let ty = Type::Meta(Meta::fresh());

                let solved_exprs = exprs
                    .iter()
                    .map(|expr| self.infer_expr(expr))
                    .collect::<InferResult<Vec<Expr>>>()?;

                for expr in &solved_exprs {
                    // println!("expr: {:?}", expr.ty);
                    self.unify(&expr.ty, &ty)?;
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
                Type::Meta(Meta::fresh()),
                pat.span,
            )),
            nir::PatternKind::Ident(name, hint) => {
                log::debug!("infer pattern ident: {:?}", name.id);
                if generalize {
                    self.ctx.insert(name.id, ty.generalize(&self.ctx));
                } else {
                    self.ctx.insert(name.id, PolyType::new(vec![], ty.clone()));
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
                let ty = Type::Meta(Meta::fresh());
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
                let ty = Type::Meta(Meta::fresh());
                let list_ty = Type::List(Box::new(ty.clone()));
                let solved_lhs = self.infer_pattern(lhs, &ty, generalize)?;
                let solved_rhs = self.infer_pattern(rhs, &list_ty, generalize)?;

                self.unify(&solved_lhs.ty, &ty)?;
                self.unify(&solved_rhs.ty, &list_ty)?;

                Ok(Pattern::new(
                    PatternKind::Pair(solved_lhs, solved_rhs),
                    list_ty,
                    pat.span,
                ))
            }
            nir::PatternKind::Unit => Ok(Pattern::new(PatternKind::Unit, Type::Unit, pat.span)),
        }
    }

    pub fn unify(&mut self, t1: &Type, t2: &Type) -> InferResult<()> {
        log::debug!("unify: {:?} and {:?}", t1, t2);
        match (&t1, &t2) {
            (Type::Byte, Type::Byte)
            | (Type::Int, Type::Int)
            | (Type::Rational, Type::Rational)
            | (Type::Real, Type::Real)
            | (Type::Bool, Type::Bool)
            | (Type::String, Type::String)
            | (Type::Char, Type::Char)
            | (Type::Unit, Type::Unit) => Ok(()),
            (Type::Lambda(p1, b1), Type::Lambda(p2, b2)) => {
                // let s1 = p1.iter().zip(p2.iter()).try_fold(
                //     Substitution::new(),
                //     |s, (t1, t2)| match t1.apply_subst(&s).unify(&t2.apply_subst(&s)) {
                //         Ok(sub) => Ok(s.compose(&sub)),
                //         err => err,
                //     },
                // )?;
                // let s2 = b1.apply_subst(&s1).unify(&b2.apply_subst(&s1))?;
                // Ok(s1.compose(&s2))
                todo!()
            }
            (Type::List(l1), Type::List(l2)) => self.unify(l1, l2),
            (_, Type::Meta(key)) => self.meta_ctx.bind(key, t1),
            (Type::Meta(key), _) => self.meta_ctx.bind(key, t2),
            _ => Err(TypeError::from(format!(
                "cannot unify {:?} and {:?}",
                t1, t2,
            ))),
        }
    }
}
