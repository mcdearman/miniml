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

        log::debug!("meta_ctx: {:#?}", self.meta_ctx);

        if errors.is_empty() {
            (
                Some(self.zonk(&Root {
                    decls,
                    span: nir.span,
                })),
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
                // self.unify(&solved_pat.ty, &solved_expr.ty)?;

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

                let meta_ret = Meta::fresh();
                let ty_ret = Type::Meta(meta_ret.clone());
                let fn_ty = Type::Lambda(param_tys.clone(), Box::new(ty_ret.clone()));
                log::debug!("decl_fn_ty: {:?}", fn_ty);

                let mut vars = param_vars.iter().cloned().collect_vec();
                vars.push(meta_ret.clone());
                self.ctx
                    .insert(name.id, PolyType::new(vars.clone(), fn_ty.clone()));
                self.ctx.push();

                let mut solved_params = vec![];
                for (ty, param) in param_tys.iter().zip(params.iter()) {
                    let solved_pat = self.infer_pattern(param, ty, false)?;
                    solved_params.push(solved_pat.clone());
                    // self.unify(&solved_pat.ty, &ty)?;
                }
                let solved_expr = self.infer_expr(fn_expr)?;
                log::debug!("unify fn expr: {:?} and {:?}", solved_expr.ty, ty_ret);
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
        log::debug!("infer expr: {:#?}", expr.kind);
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
                log::debug!("app_solved_fun: {:?}", solved_fun.ty);

                // Γ ⊢ e1 : T1
                // let solved_arg = self.infer_expr(arg)?;
                let solved_args = args
                    .iter()
                    .map(|arg| self.infer_expr(arg))
                    .collect::<InferResult<Vec<Expr>>>()?;
                log::debug!(
                    "app_solved_args: {:?}",
                    solved_args
                        .clone()
                        .into_iter()
                        .map(|arg| arg.ty)
                        .collect_vec()
                );

                // T' = newvar
                let ty_ret = Type::Meta(Meta::fresh());
                log::debug!("app_ty_ret: {:?}", ty_ret);

                // unify(T0, T1 -> T')
                let ty_args = solved_args.iter().map(|arg| arg.ty.clone()).collect_vec();
                log::debug!("app_ty_args: {:?}", ty_args);
                let ty_fun = Type::Lambda(ty_args, Box::new(ty_ret.clone()));
                log::debug!("app_ty_fun: {:?}", ty_fun);
                self.unify(&solved_fun.ty, &ty_fun)?;
                log::debug!("exit apply");

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
                log::debug!(
                    "unify let pat: {:?} and {:?}",
                    solved_pat.ty,
                    solved_expr.ty
                );
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
                    log::debug!("unify fn param: {:?} and {:?}", solved_pat.ty, ty);
                    self.unify(&solved_pat.ty, &ty)?;
                }

                let solved_expr = self.infer_expr(expr)?;
                // log::debug!("fn_solved_expr: {:?}", solved_expr.ty);
                log::debug!("unify fn expr: {:?} and {:?}", solved_expr.ty, ty_ret);
                self.unify(&solved_expr.ty, &ty_ret)?;
                self.ctx.pop();

                let solved_body = self.infer_expr(body)?;
                // log::debug!("fn_solved_body: {:?}", solved_body.ty);
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
                    log::debug!(
                        "unify match pat: {:?} and {:?}",
                        solved_pat.ty,
                        solved_expr.ty
                    );
                    self.unify(&solved_pat.ty, &solved_expr.ty)?;

                    let solved_body = self.infer_expr(body)?;
                    log::debug!(
                        "unify match body: {:?} and {:?}",
                        solved_body.ty,
                        solved_expr.ty
                    );
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
                    log::debug!("unify list: {:?} and {:?}", expr.ty, ty);
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
                    let scm = ty.generalize(&self.ctx);
                    log::debug!("scm: {:?}", scm);
                    self.ctx.insert(name.id, scm);
                } else {
                    self.ctx.insert(name.id, PolyType::new(vec![], ty.clone()));
                }
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
                log::debug!("infer list pat: {:?}", ty);
                let elem_ty = Type::Meta(Meta::fresh());
                let list_ty = Type::List(Box::new(elem_ty.clone()));
                let solved_pats = pats
                    .iter()
                    .map(|pat| self.infer_pattern(pat, &elem_ty, generalize))
                    .collect::<InferResult<Vec<Pattern>>>()?;
                log::debug!("unify list: {:?} and {:?}", ty, list_ty);
                self.unify(ty, &list_ty)?;
                // log::debug!("solved_pats: {:?}", solved_pats);

                Ok(Pattern::new(
                    PatternKind::List(solved_pats),
                    list_ty,
                    pat.span,
                ))
            }
            nir::PatternKind::Pair(lhs, rhs) => {
                let elem_ty = Type::Meta(Meta::fresh());
                let list_ty = Type::List(Box::new(elem_ty.clone()));
                let solved_lhs = self.infer_pattern(lhs, &elem_ty, generalize)?;
                let solved_rhs = self.infer_pattern(rhs, &list_ty, generalize)?;

                // self.unify(&solved_lhs.ty, &elem_ty)?;
                log::debug!("unify pair: {:?} and {:?}", ty, list_ty);
                self.unify(ty, &list_ty)?;
                // self.unify(&solved_rhs.ty, &list_ty)?;

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
                if p1.len() != p2.len() {
                    return Err(TypeError::from(format!(
                        "functions have different arity: {:?} and {:?}",
                        t1, t2,
                    )));
                }
                for (t1, t2) in p1.iter().zip(p2.iter()) {
                    self.unify(t1, t2)?;
                }
                self.unify(b1, b2)
            }
            (Type::List(l1), Type::List(l2)) => self.unify(l1, l2),
            (_, Type::Meta(key)) => {
                self.meta_ctx.bind(key, t1)?;
                log::debug!("bind: {:?} to {:?}", key, t1);
                log::debug!("meta_ctx: {:#?}", self.meta_ctx);
                Ok(())
            }
            (Type::Meta(key), _) => {
                self.meta_ctx.bind(key, t2)?;
                log::debug!("bind: {:?} to {:?}", key, t2);
                log::debug!("meta_ctx: {:#?}", self.meta_ctx);
                Ok(())
            }
            _ => Err(TypeError::from(format!(
                "cannot unify {:?} and {:?}",
                t1, t2,
            ))),
        }
    }

    fn zonk(&self, root: &Root) -> Root {
        let decls = root
            .decls
            .iter()
            .map(|decl| self.zonk_decl(decl))
            .collect_vec();
        Root {
            decls,
            span: root.span,
        }
    }

    fn zonk_decl(&self, decl: &Decl) -> Decl {
        match &decl.kind {
            DeclKind::Let(pat, let_expr) => {
                let zonked_pat = self.zonk_pattern(pat);
                let zonked_expr = self.zonk_expr(let_expr);
                Decl {
                    kind: DeclKind::Let(zonked_pat, zonked_expr),
                    ty: self.zonk_type(&decl.ty),
                    span: decl.span,
                }
            }
            DeclKind::Fn(name, params, fn_expr) => {
                let zonked_params = params
                    .iter()
                    .map(|param| self.zonk_pattern(param))
                    .collect_vec();
                let zonked_expr = self.zonk_expr(fn_expr);
                Decl {
                    kind: DeclKind::Fn(*name, zonked_params, zonked_expr),
                    ty: self.zonk_type(&decl.ty),
                    span: decl.span,
                }
            }
        }
    }

    fn zonk_expr(&self, expr: &Expr) -> Expr {
        match expr.kind.as_ref() {
            ExprKind::Lit(lit) => Expr::new(
                ExprKind::Lit(lit.clone()),
                self.zonk_type(&expr.ty),
                expr.span,
            ),
            ExprKind::Var(name) => {
                Expr::new(ExprKind::Var(*name), self.zonk_type(&expr.ty), expr.span)
            }
            ExprKind::Lambda(params, fn_expr) => {
                let zonked_params = params
                    .iter()
                    .map(|param| self.zonk_pattern(param))
                    .collect_vec();
                let zonked_expr = self.zonk_expr(fn_expr);
                Expr::new(
                    ExprKind::Lambda(zonked_params, zonked_expr),
                    self.zonk_type(&expr.ty),
                    expr.span,
                )
            }
            ExprKind::Apply(fun, args) => {
                let zonked_fun = self.zonk_expr(fun);
                let zonked_args = args.iter().map(|arg| self.zonk_expr(arg)).collect_vec();
                Expr::new(
                    ExprKind::Apply(zonked_fun, zonked_args),
                    self.zonk_type(&expr.ty),
                    expr.span,
                )
            }
            ExprKind::Or(lhs, rhs) => {
                let zonked_lhs = self.zonk_expr(lhs);
                let zonked_rhs = self.zonk_expr(rhs);
                Expr::new(
                    ExprKind::Or(zonked_lhs, zonked_rhs),
                    self.zonk_type(&expr.ty),
                    expr.span,
                )
            }
            ExprKind::And(lhs, rhs) => {
                let zonked_lhs = self.zonk_expr(lhs);
                let zonked_rhs = self.zonk_expr(rhs);
                Expr::new(
                    ExprKind::And(zonked_lhs, zonked_rhs),
                    self.zonk_type(&expr.ty),
                    expr.span,
                )
            }
            ExprKind::Let(pat, let_expr, body) => {
                let zonked_pat = self.zonk_pattern(pat);
                let zonked_let_expr = self.zonk_expr(let_expr);
                let zonked_body = self.zonk_expr(body);
                Expr::new(
                    ExprKind::Let(zonked_pat, zonked_let_expr, zonked_body),
                    self.zonk_type(&expr.ty),
                    expr.span,
                )
            }
            ExprKind::Fn(name, params, expr, body) => {
                let zonked_params = params
                    .iter()
                    .map(|param| self.zonk_pattern(param))
                    .collect_vec();
                let zonked_expr = self.zonk_expr(expr);
                let zonked_body = self.zonk_expr(body);
                Expr::new(
                    ExprKind::Fn(*name, zonked_params, zonked_expr, zonked_body),
                    self.zonk_type(&expr.ty),
                    expr.span,
                )
            }
            ExprKind::If(cond, then, else_) => {
                let zonked_cond = self.zonk_expr(cond);
                let zonked_then = self.zonk_expr(then);
                let zonked_else = self.zonk_expr(else_);
                Expr::new(
                    ExprKind::If(zonked_cond, zonked_then, zonked_else),
                    self.zonk_type(&expr.ty),
                    expr.span,
                )
            }
            ExprKind::Match(expr, arms) => {
                let zonked_expr = self.zonk_expr(expr);
                let zonked_arms = arms
                    .iter()
                    .map(|(pat, body)| {
                        let zonked_pat = self.zonk_pattern(pat);
                        let zonked_body = self.zonk_expr(body);
                        (zonked_pat, zonked_body)
                    })
                    .collect_vec();
                Expr::new(
                    ExprKind::Match(zonked_expr, zonked_arms),
                    self.zonk_type(&expr.ty),
                    expr.span,
                )
            }
            ExprKind::List(exprs) => {
                let zonked_exprs = exprs.iter().map(|expr| self.zonk_expr(expr)).collect_vec();
                Expr::new(
                    ExprKind::List(zonked_exprs),
                    self.zonk_type(&expr.ty),
                    expr.span,
                )
            }
            ExprKind::Unit => Expr::new(ExprKind::Unit, self.zonk_type(&expr.ty), expr.span),
        }
    }

    fn zonk_pattern(&self, pat: &Pattern) -> Pattern {
        match pat.kind.as_ref() {
            PatternKind::Wildcard => {
                Pattern::new(PatternKind::Wildcard, self.zonk_type(&pat.ty), pat.span)
            }
            PatternKind::Ident(name) => {
                Pattern::new(PatternKind::Ident(*name), self.zonk_type(&pat.ty), pat.span)
            }
            PatternKind::Lit(lit) => Pattern::new(
                PatternKind::Lit(lit.clone()),
                self.zonk_type(&pat.ty),
                pat.span,
            ),
            PatternKind::List(pats) => {
                let zonked_pats = pats.iter().map(|pat| self.zonk_pattern(pat)).collect_vec();
                Pattern::new(
                    PatternKind::List(zonked_pats),
                    self.zonk_type(&pat.ty),
                    pat.span,
                )
            }
            PatternKind::Pair(lhs, rhs) => {
                let zonked_lhs = self.zonk_pattern(lhs);
                let zonked_rhs = self.zonk_pattern(rhs);
                Pattern::new(
                    PatternKind::Pair(zonked_lhs, zonked_rhs),
                    self.zonk_type(&pat.ty),
                    pat.span,
                )
            }
            PatternKind::Unit => Pattern::new(PatternKind::Unit, self.zonk_type(&pat.ty), pat.span),
        }
    }

    fn zonk_type(&self, ty: &Type) -> Type {
        match ty {
            Type::Byte => Type::Byte,
            Type::Int => Type::Int,
            Type::Rational => Type::Rational,
            Type::Real => Type::Real,
            Type::Bool => Type::Bool,
            Type::String => Type::String,
            Type::Char => Type::Char,
            Type::Unit => Type::Unit,
            Type::Meta(meta) => self
                .meta_ctx
                .get(&meta.id())
                .cloned()
                .unwrap_or_else(|| ty.clone()),
            Type::Lambda(params, body) => Type::Lambda(
                params.iter().map(|ty| self.zonk_type(ty)).collect_vec(),
                Box::new(self.zonk_type(body)),
            ),
            Type::List(ty) => Type::List(Box::new(self.zonk_type(ty))),
            Type::Record(id, fields) => Type::Record(
                *id,
                fields
                    .iter()
                    .map(|(name, ty)| (name.clone(), self.zonk_type(ty)))
                    .collect(),
            ),
            Type::Poly(poly) => Type::Poly(poly.clone()),
        }
    }
}
