use self::{
    context::Context,
    error::{InferResult, TypeError},
    meta_context::MetaContext,
    tir::*,
    ty::Ty,
};
use crate::{rename::nir, utils::intern::InternedString};
use constraint::Constraint;
use itertools::Itertools;
use libc::ABDAY_6;
use poly_type::PolyType;
use registry::Registry;

mod constraint;
mod context;
pub mod error;
mod meta;
mod meta_context;
mod poly_type;
pub mod registry;
pub mod tests;
pub mod tir;
pub mod ty;

#[derive(Debug, Clone)]
pub struct TypeSolver {
    src: InternedString,
    ctx: Context,
    meta_ctx: MetaContext,
    reg: Registry,
    errors: Vec<TypeError>,
}

impl TypeSolver {
    pub fn new() -> Self {
        let mut meta_ctx = MetaContext::new();
        let mut ctx = Context::new();

        ctx.insert(
            "main".into(),
            PolyType::new(
                vec![],
                Ty::Lambda(Box::new(Ty::Array(Box::new(Ty::String))), Box::new(Ty::Int)),
            ),
        );

        ctx.insert(
            "__neg__".into(),
            PolyType::new(vec![], Ty::Lambda(Box::new(Ty::Int), Box::new(Ty::Int))),
        );

        ctx.insert(
            "__not__".into(),
            PolyType::new(vec![], Ty::Lambda(Box::new(Ty::Bool), Box::new(Ty::Bool))),
        );

        ctx.insert(
            "__add__".into(),
            PolyType::new(
                vec![],
                Ty::Lambda(
                    Box::new(Ty::Int),
                    Box::new(Ty::Lambda(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__sub__".into(),
            PolyType::new(
                vec![],
                Ty::Lambda(
                    Box::new(Ty::Int),
                    Box::new(Ty::Lambda(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__mul__".into(),
            PolyType::new(
                vec![],
                Ty::Lambda(
                    Box::new(Ty::Int),
                    Box::new(Ty::Lambda(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__div__".into(),
            PolyType::new(
                vec![],
                Ty::Lambda(
                    Box::new(Ty::Int),
                    Box::new(Ty::Lambda(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__rem__".into(),
            PolyType::new(
                vec![],
                Ty::Lambda(
                    Box::new(Ty::Int),
                    Box::new(Ty::Lambda(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__pow__".into(),
            PolyType::new(
                vec![],
                Ty::Lambda(
                    Box::new(Ty::Int),
                    Box::new(Ty::Lambda(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__eq__".into(),
            PolyType::new(
                vec![],
                Ty::Lambda(
                    Box::new(Ty::Int),
                    Box::new(Ty::Lambda(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        ctx.insert(
            "__neq__".into(),
            PolyType::new(
                vec![],
                Ty::Lambda(
                    Box::new(Ty::Int),
                    Box::new(Ty::Lambda(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        ctx.insert(
            "__lt__".into(),
            PolyType::new(
                vec![],
                Ty::Lambda(
                    Box::new(Ty::Int),
                    Box::new(Ty::Lambda(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        ctx.insert(
            "__lte__".into(),
            PolyType::new(
                vec![],
                Ty::Lambda(
                    Box::new(Ty::Int),
                    Box::new(Ty::Lambda(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        ctx.insert(
            "__gt__".into(),
            PolyType::new(
                vec![],
                Ty::Lambda(
                    Box::new(Ty::Int),
                    Box::new(Ty::Lambda(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        ctx.insert(
            "__gte__".into(),
            PolyType::new(
                vec![],
                Ty::Lambda(
                    Box::new(Ty::Int),
                    Box::new(Ty::Lambda(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        let r = meta_ctx.fresh();
        ctx.insert(
            "__pair__".into(),
            Ty::Lambda(
                Box::new(Ty::MetaRef(r)),
                Box::new(Ty::Lambda(
                    Box::new(Ty::List(Box::new(Ty::MetaRef(r)))),
                    Box::new(Ty::List(Box::new(Ty::MetaRef(r)))),
                )),
            )
            .generalize(&ctx, &meta_ctx),
        );

        Self {
            src: "".into(),
            ctx,
            meta_ctx,
            reg: Registry::new(),
        }
    }

    fn generate_constraints<'src>(&mut self, src: &'src str, nir: &nir::Prog) -> Prog {
        Prog {
            decls: nir
                .decls
                .iter()
                .map(|decl| self.generate_decl_constraints(src, decl))
                .collect_vec(),
            span: nir.span,
        }
    }

    fn generate_decl_constraints<'src>(&mut self, src: &'src str, decl: &nir::Decl) -> Decl {
        match &decl.kind {
            nir::DeclKind::Def(pat, _, expr) => {
                let solved_expr = self.generate_expr_constraints(src, expr);
                let solved_pat = self.generate_pattern_constraints(src, pat, &solved_expr.ty, true);

                Decl {
                    kind: DeclKind::Def(solved_pat, solved_expr.clone()),
                    ty: solved_expr.ty,
                    span: decl.span,
                }
            }
        }
    }

    fn generate_expr_constraints<'src>(
        &mut self,
        src: &'src str,
        expr: &nir::Expr,
    ) -> Option<Expr> {
        match expr.kind.as_ref() {
            nir::ExprKind::Lit(lit) => match lit {
                nir::Lit::Byte(b) => {
                    Some(Expr::new(ExprKind::Lit(Lit::Byte(*b)), Ty::Byte, expr.span))
                }
                nir::Lit::Int(i) => {
                    Some(Expr::new(ExprKind::Lit(Lit::Int(*i)), Ty::Int, expr.span))
                }
                nir::Lit::Rational(r) => Some(Expr::new(
                    ExprKind::Lit(Lit::Rational(*r)),
                    Ty::Rational,
                    expr.span,
                )),
                nir::Lit::Real(r) => {
                    Some(Expr::new(ExprKind::Lit(Lit::Real(*r)), Ty::Real, expr.span))
                }
                nir::Lit::Bool(b) => {
                    Some(Expr::new(ExprKind::Lit(Lit::Bool(*b)), Ty::Bool, expr.span))
                }
                nir::Lit::String(s) => Some(Expr::new(
                    ExprKind::Lit(Lit::String(s.clone())),
                    Ty::String,
                    expr.span,
                )),
                nir::Lit::Char(c) => {
                    Some(Expr::new(ExprKind::Lit(Lit::Char(*c)), Ty::Char, expr.span))
                }
            },
            nir::ExprKind::Var(ident) => {
                if let Some(scm) = self.ctx.get(&ident.name) {
                    Some(Expr::new(
                        ExprKind::Var(*ident),
                        scm.instantiate(&mut self.meta_ctx),
                        expr.span,
                    ))
                } else {
                    self.errors.push(TypeError::from(format!(
                        "unbound variable: {:?} - \"{}\"",
                        expr,
                        self.src[ident.span].to_string()
                    )));
                    None
                }
            }
            nir::ExprKind::Apply(fun, arg) => todo!(),
            nir::ExprKind::Lambda(pattern, expr) => todo!(),
            nir::ExprKind::Or(expr, expr1) => todo!(),
            nir::ExprKind::And(expr, expr1) => todo!(),
            nir::ExprKind::Let(pattern, _, expr, expr1) => todo!(),
            nir::ExprKind::If(expr, expr1, expr2) => todo!(),
            nir::ExprKind::Match(expr, vec) => todo!(),
            nir::ExprKind::List(vec) => todo!(),
            nir::ExprKind::Unit => todo!(),
        }
    }

    fn generate_pattern_constraints<'src>(
        &mut self,
        src: &'src str,
        pat: &nir::Pattern,
        ty: &Ty,
        generalize: bool,
    ) -> Pattern {
        todo!()
    }

    pub fn infer<'src>(
        &mut self,
        src: &'src str,
        nir: &nir::Prog,
    ) -> (Option<Prog>, Vec<TypeError>) {
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
        self.ctx.zonk(&mut self.meta_ctx);
        log::debug!("ctx: {:#?}", self.ctx);

        if errors.is_empty() {
            (
                Some(
                    Prog {
                        decls,
                        span: nir.span,
                    }
                    .zonk(&mut self.meta_ctx),
                ),
                errors,
            )
        } else {
            (None, errors)
        }
    }

    fn infer_decl(&mut self, decl: &nir::Decl) -> InferResult<Decl> {
        match &decl.kind {
            nir::DeclKind::Def(pat, false, expr) => {
                log::debug!("infer def ({:?})", pat);
                // to show that Γ ⊢ let x = e0: T we need to show that
                // Γ ⊢ e0 : T
                let solved_expr = self.infer_expr(expr)?;

                // Γ, x: gen(T) ⊢ e0 : T
                let solved_pat = self.infer_pattern(pat, &solved_expr.ty, true)?;

                Ok(Decl {
                    kind: DeclKind::Def(solved_pat, solved_expr.clone()),
                    ty: solved_expr.ty,
                    span: decl.span,
                })
            }
            nir::DeclKind::Def(pat, true, expr) => {
                log::debug!("infer def rec ({:?})", pat);
                let binder_ty = Ty::MetaRef(self.meta_ctx.fresh());
                log::debug!("def_binder_ty: {:?}", binder_ty);

                match pat.kind.as_ref() {
                    nir::PatternKind::Ident(ident, _) => {
                        self.ctx
                            .insert(ident.name, PolyType::new(vec![], binder_ty.clone()));
                    }
                    _ => {
                        return Err(TypeError::from(format!(
                            "recursion is only allowed with identifiers: {:?}",
                            pat
                        )));
                    }
                }

                let solved_expr = self.infer_expr(expr)?;
                log::debug!("def_solved_expr: {:?}", solved_expr.ty);

                let solved_pat = self.infer_pattern(pat, &solved_expr.ty, true)?;
                log::debug!("def_solved_pat: {:?}", solved_pat.ty);

                Ok(Decl {
                    kind: DeclKind::Def(solved_pat, solved_expr.clone()),
                    ty: solved_expr.ty,
                    span: decl.span,
                })
            }
        }
    }

    fn infer_expr(&mut self, expr: &nir::Expr) -> InferResult<Expr> {
        log::debug!("infer expr: {:?}", expr.span);
        match expr.kind.as_ref() {
            nir::ExprKind::Lit(lit) => {
                log::debug!("infer lit");
                match *lit {
                    nir::Lit::Byte(b) => {
                        Ok(Expr::new(ExprKind::Lit(Lit::Byte(b)), Ty::Byte, expr.span))
                    }
                    nir::Lit::Int(n) => {
                        Ok(Expr::new(ExprKind::Lit(Lit::Int(n)), Ty::Int, expr.span))
                    }
                    nir::Lit::Rational(r) => Ok(Expr::new(
                        ExprKind::Lit(Lit::Rational(r)),
                        Ty::Rational,
                        expr.span,
                    )),
                    nir::Lit::Real(r) => {
                        Ok(Expr::new(ExprKind::Lit(Lit::Real(r)), Ty::Real, expr.span))
                    }
                    nir::Lit::Bool(b) => {
                        Ok(Expr::new(ExprKind::Lit(Lit::Bool(b)), Ty::Bool, expr.span))
                    }
                    nir::Lit::String(s) => Ok(Expr::new(
                        ExprKind::Lit(Lit::String(s.clone())),
                        Ty::String,
                        expr.span,
                    )),
                    nir::Lit::Char(c) => {
                        Ok(Expr::new(ExprKind::Lit(Lit::Char(c)), Ty::Char, expr.span))
                    }
                }
            }
            nir::ExprKind::Var(ident) => {
                // to show that Γ ⊢ x : T we need to show that

                // x : σ ∈ Γ
                log::debug!("infer var: {:?}", ident.name);
                if let Some(scm) = self.ctx.get(&ident.name) {
                    // T = inst(σ)
                    log::debug!("var_scm: {:?}", scm);
                    let ty = scm.instantiate(&mut self.meta_ctx);
                    log::debug!("var_ty: {:?}", ty);

                    Ok(Expr::new(ExprKind::Var(*ident), ty, expr.span))
                } else {
                    Err(TypeError::from(format!(
                        "unbound variable: {:?} - \"{}\"",
                        expr,
                        self.src[ident.span].to_string()
                    )))
                }
            }
            nir::ExprKind::Lambda(param, fn_expr) => {
                log::debug!("infer lambda");
                // to show that Γ ⊢ λx.e : T -> T' we need to show that
                // T = newvar

                // Γ, x : T ⊢ e : T'
                let param_ty = Ty::MetaRef(self.meta_ctx.fresh());
                log::debug!("lambda_param_type: {:?}", param_ty);
                self.ctx.push();
                let solved_param = self.infer_pattern(param, &param_ty, false)?;

                let solved_expr = self.infer_expr(fn_expr)?;
                let fun_ty = Ty::Lambda(Box::new(param_ty), Box::new(solved_expr.ty.clone()));
                log::debug!("lambda_fun_ty: {:?}", fun_ty);
                self.ctx.pop();

                Ok(Expr::new(
                    ExprKind::Lambda(solved_param, solved_expr),
                    fun_ty,
                    expr.span,
                ))
            }
            nir::ExprKind::Apply(fun, arg) => {
                log::debug!("infer app: {:?}", fun.span);
                // to show that Γ ⊢ e0 e1 : T' we need to show that
                // Γ ⊢ e0 : T0
                let solved_fun = self.infer_expr(fun)?;
                log::debug!("app_solved_fun: {:?}", solved_fun.ty);

                // Γ ⊢ e1 : T1
                let solved_arg = self.infer_expr(arg)?;
                log::debug!("app_solved_arg: {:?}", solved_arg.ty);

                // T' = newvar
                let ty_ret = Ty::MetaRef(self.meta_ctx.fresh());
                log::debug!("app_ty_ret: {:?}", ty_ret);

                // unify(T0, T1 -> T')
                let ty_fun = Ty::Lambda(Box::new(solved_arg.clone().ty), Box::new(ty_ret.clone()));
                log::debug!("app_ty_fun: {:?}", ty_fun);
                self.meta_ctx.unify(&solved_fun.ty, &ty_fun)?;
                log::debug!("exit apply");

                Ok(Expr::new(
                    ExprKind::Apply(solved_fun, solved_arg),
                    ty_ret,
                    expr.span,
                ))
            }
            nir::ExprKind::Or(lhs, rhs) => {
                let solved_lhs = self.infer_expr(lhs)?;
                let solved_rhs = self.infer_expr(rhs)?;

                self.meta_ctx.unify(&solved_lhs.ty, &Ty::Bool)?;
                self.meta_ctx.unify(&solved_rhs.ty, &Ty::Bool)?;

                Ok(Expr::new(
                    ExprKind::Or(solved_lhs, solved_rhs),
                    Ty::Bool,
                    expr.span,
                ))
            }
            nir::ExprKind::And(lhs, rhs) => {
                let solved_lhs = self.infer_expr(lhs)?;
                let solved_rhs = self.infer_expr(rhs)?;

                self.meta_ctx.unify(&solved_lhs.ty, &Ty::Bool)?;
                self.meta_ctx.unify(&solved_rhs.ty, &Ty::Bool)?;

                Ok(Expr::new(
                    ExprKind::And(solved_lhs, solved_rhs),
                    Ty::Bool,
                    expr.span,
                ))
            }
            nir::ExprKind::Let(pat, rec, let_expr, body) => {
                log::debug!("infer let ({:?})", pat.span);
                // to show that Γ ⊢ let x = e0 in e1 : T' we need to show that
                // Γ ⊢ e0 : T
                if *rec {
                    let binder_ty = Ty::MetaRef(self.meta_ctx.fresh());
                    log::debug!("let_binder_ty: {:?}", binder_ty);

                    match pat.kind.as_ref() {
                        nir::PatternKind::Ident(ident, _) => {
                            self.ctx
                                .insert(ident.name, PolyType::new(vec![], binder_ty.clone()));
                        }
                        _ => {
                            return Err(TypeError::from(format!(
                                "recursion is only allowed with identifiers: {:?}",
                                pat
                            )));
                        }
                    }

                    let solved_expr = self.infer_expr(let_expr)?;
                    log::debug!("let_solved_expr: {:?}", solved_expr.ty);

                    let solved_pat = self.infer_pattern(pat, &solved_expr.ty, false)?;
                    log::debug!("let_solved_pat: {:?}", solved_pat.ty);

                    let solved_body = self.infer_expr(body)?;
                    log::debug!("let_solved_body: {:?}", solved_body.ty);

                    Ok(Expr::new(
                        ExprKind::Let(solved_pat, *rec, solved_expr, solved_body.clone()),
                        solved_body.ty,
                        expr.span,
                    ))
                } else {
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
                    // self.meta_ctx.unify(&solved_pat.ty, &solved_expr.ty)?;

                    let solved_body = self.infer_expr(body)?;

                    Ok(Expr::new(
                        ExprKind::Let(solved_pat, *rec, solved_expr, solved_body.clone()),
                        solved_body.ty,
                        expr.span,
                    ))
                }
            }
            nir::ExprKind::If(cond, then, else_) => {
                log::debug!("infer if");
                let solved_cond = self.infer_expr(cond)?;
                let solved_then = self.infer_expr(then)?;
                let solved_else = self.infer_expr(else_)?;

                self.meta_ctx.unify(&solved_cond.ty, &Ty::Bool)?;
                self.meta_ctx.unify(&solved_then.ty, &solved_else.ty)?;

                Ok(Expr::new(
                    ExprKind::If(solved_cond, solved_then.clone(), solved_else),
                    solved_then.ty,
                    expr.span,
                ))
            }
            nir::ExprKind::Match(expr, arms) => {
                log::debug!("infer match");

                let solved_expr = self.infer_expr(expr)?;
                // let ty = Type::MetaRef(self.meta_ctx.fresh());
                log::debug!("match_ty: {:?}", solved_expr.ty);

                let mut solved_arms = vec![];
                for (pat, body) in arms {
                    self.ctx.push();

                    let solved_pat = self.infer_pattern(pat, &solved_expr.ty, false)?;
                    let solved_body = self.infer_expr(body)?;

                    log::debug!(
                        "unify match body: {:?} and {:?}",
                        solved_body.ty,
                        solved_expr.ty.clone()
                    );
                    self.meta_ctx.unify(&solved_body.ty, &solved_expr.ty)?;

                    solved_arms.push((solved_pat, solved_body));

                    self.ctx.pop();
                }

                Ok(Expr::new(
                    ExprKind::Match(solved_expr.clone(), solved_arms),
                    solved_expr.ty,
                    expr.span,
                ))
            }
            nir::ExprKind::List(exprs) => {
                let ty = Ty::MetaRef(self.meta_ctx.fresh());

                let solved_exprs = exprs
                    .iter()
                    .map(|expr| self.infer_expr(expr))
                    .collect::<InferResult<Vec<Expr>>>()?;

                for expr in &solved_exprs {
                    log::debug!("unify list: {:?} and {:?}", expr.ty, ty);
                    self.meta_ctx.unify(&expr.ty, &ty)?;
                }

                Ok(Expr::new(
                    ExprKind::List(solved_exprs),
                    Ty::List(Box::new(ty)),
                    expr.span,
                ))
            }
            nir::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, Ty::Unit, expr.span)),
        }
    }

    fn infer_pattern(
        &mut self,
        pat: &nir::Pattern,
        ty: &Ty,
        generalize: bool,
    ) -> InferResult<Pattern> {
        match pat.kind.as_ref() {
            nir::PatternKind::Wildcard => Ok(Pattern::new(
                PatternKind::Wildcard,
                Ty::MetaRef(self.meta_ctx.fresh()),
                pat.span,
            )),
            nir::PatternKind::Ident(ident, hint) => {
                log::debug!("infer pattern ident: {:?}", ident.name);
                if generalize {
                    let scm = ty.generalize(&self.ctx, &self.meta_ctx);
                    log::debug!("gen scm: {:?}", scm);
                    self.ctx.insert(ident.name, scm.clone());
                    Ok(Pattern::new(
                        PatternKind::Ident(*ident),
                        ty.clone(),
                        pat.span,
                    ))
                } else {
                    self.ctx
                        .insert(ident.name, PolyType::new(vec![], ty.clone()));
                    Ok(Pattern::new(
                        PatternKind::Ident(*ident),
                        ty.clone(),
                        pat.span,
                    ))
                }
            }
            nir::PatternKind::Lit(lit) => match *lit {
                nir::Lit::Byte(b) => Ok(Pattern::new(
                    PatternKind::Lit(Lit::Byte(b)),
                    Ty::Byte,
                    pat.span,
                )),
                nir::Lit::Int(n) => Ok(Pattern::new(
                    PatternKind::Lit(Lit::Int(n)),
                    Ty::Int,
                    pat.span,
                )),
                nir::Lit::Rational(r) => Ok(Pattern::new(
                    PatternKind::Lit(Lit::Rational(r)),
                    Ty::Rational,
                    pat.span,
                )),
                nir::Lit::Real(r) => Ok(Pattern::new(
                    PatternKind::Lit(Lit::Real(r)),
                    Ty::Real,
                    pat.span,
                )),
                nir::Lit::Bool(b) => Ok(Pattern::new(
                    PatternKind::Lit(Lit::Bool(b)),
                    Ty::Bool,
                    pat.span,
                )),
                nir::Lit::String(s) => Ok(Pattern::new(
                    PatternKind::Lit(Lit::String(s.clone())),
                    Ty::String,
                    pat.span,
                )),
                nir::Lit::Char(c) => Ok(Pattern::new(
                    PatternKind::Lit(Lit::Char(c)),
                    Ty::Char,
                    pat.span,
                )),
            },
            nir::PatternKind::List(pats) => {
                log::debug!("infer list pat: {:?}", ty);
                let elem_ty = Ty::MetaRef(self.meta_ctx.fresh());
                let list_ty = Ty::List(Box::new(elem_ty.clone()));
                let solved_pats = pats
                    .iter()
                    .map(|pat| self.infer_pattern(pat, &elem_ty, generalize))
                    .collect::<InferResult<Vec<Pattern>>>()?;
                log::debug!("unify list: {:?} and {:?}", ty, list_ty);
                self.meta_ctx.unify(ty, &list_ty)?;
                // log::debug!("solved_pats: {:?}", solved_pats);

                Ok(Pattern::new(
                    PatternKind::List(solved_pats),
                    list_ty,
                    pat.span,
                ))
            }
            nir::PatternKind::Pair(lhs, rhs) => {
                let elem_ty = Ty::MetaRef(self.meta_ctx.fresh());
                let list_ty = Ty::List(Box::new(elem_ty.clone()));
                let solved_lhs = self.infer_pattern(lhs, &elem_ty, generalize)?;
                let solved_rhs = self.infer_pattern(rhs, &list_ty, generalize)?;

                // self.meta_ctx.unify(&solved_lhs.ty, &elem_ty)?;
                log::debug!("unify pair: {:?} and {:?}", ty, list_ty);
                self.meta_ctx.unify(ty, &list_ty)?;
                // self.meta_ctx.unify(&solved_rhs.ty, &list_ty)?;

                Ok(Pattern::new(
                    PatternKind::Pair(solved_lhs, solved_rhs),
                    list_ty,
                    pat.span,
                ))
            }
            nir::PatternKind::Unit => Ok(Pattern::new(PatternKind::Unit, Ty::Unit, pat.span)),
        }
    }
}
