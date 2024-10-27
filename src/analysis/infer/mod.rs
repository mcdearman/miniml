use self::{
    context::Context,
    error::{InferResult, TypeError},
    meta_context::MetaContext,
    tir::*,
    ty::Ty,
};
use crate::{rename::nir, utils::intern::InternedString};
use chumsky::container::Seq;
use clap::builder::TryMapValueParser;
use constraint::Constraint;
use itertools::Itertools;
use poly_type::PolyType;

mod constraint;
mod context;
pub mod error;
mod meta;
mod meta_context;
mod poly_type;
pub mod tests;
pub mod tir;
pub mod ty;

#[derive(Debug, Clone)]
pub struct TypeSolver {
    src: InternedString,
    pub ctx: Context,
    pub meta_ctx: MetaContext,
    constraints: Vec<Constraint>,
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
            constraints: vec![],
        }
    }

    pub fn infer<'src>(&mut self, src: &'src str, nir: &nir::Prog) -> (Prog, Vec<TypeError>) {
        let (prog, gen_errors) = self.generate_constraints(src, nir);
        // log::debug!("prog: {:#?}", prog);
        log::debug!("constraints: {:#?}", self.constraints);
        let unify_errors = self.solve_constraints();
        let errors = gen_errors.into_iter().chain(unify_errors).collect_vec();
        self.constraints.clear();
        (prog.zonk(&mut self.meta_ctx), errors)
    }

    fn solve_constraints<'src>(&mut self) -> Vec<TypeError> {
        let mut errors = vec![];
        for c in &self.constraints {
            match c {
                Constraint::Eq(t1, t2) => {
                    if let Err(e) = self.meta_ctx.unify(&t1, &t2) {
                        errors.push(e);
                    }
                }
            }
        }
        errors
    }

    fn generate_constraints<'src>(
        &mut self,
        src: &'src str,
        nir: &nir::Prog,
    ) -> (Prog, Vec<TypeError>) {
        let mut decls = vec![];
        let mut errors = vec![];

        for decl in &nir.decls {
            match self.generate_decl_constraints(src, decl) {
                Ok(decl) => {
                    decls.push(decl);
                }
                Err(e) => errors.push(e),
            }
        }

        (
            Prog {
                decls,
                span: nir.span,
            },
            errors,
        )
    }

    fn generate_decl_constraints<'src>(
        &mut self,
        src: &'src str,
        decl: &nir::Decl,
    ) -> InferResult<Decl> {
        match &decl.kind {
            nir::DeclKind::Def(pat, false, expr) => {
                let solved_expr = self.generate_expr_constraints(src, expr)?;
                let solved_pat =
                    self.generate_pattern_constraints(src, pat, &solved_expr.ty, true)?;

                Ok(Decl {
                    kind: DeclKind::Def(solved_pat, false, solved_expr.clone()),
                    ty: solved_expr.ty,
                    span: decl.span,
                })
            }
            nir::DeclKind::Def(pat, true, expr) => {
                let var = Ty::MetaRef(self.meta_ctx.fresh());
                log::debug!("fresh def var: {:?}", var);
                let solved_pat = self.generate_pattern_constraints(src, pat, &var, true)?;

                self.ctx.push();
                let solved_expr = self.generate_expr_constraints(src, expr)?;
                self.ctx.pop();

                self.constraints.push(Constraint::Eq(
                    solved_pat.ty.clone(),
                    solved_expr.ty.clone(),
                ));

                Ok(Decl {
                    kind: DeclKind::Def(solved_pat, true, solved_expr.clone()),
                    ty: var,
                    span: decl.span,
                })
            }
        }
    }

    fn generate_expr_constraints<'src>(
        &mut self,
        src: &'src str,
        expr: &nir::Expr,
    ) -> InferResult<Expr> {
        match expr.kind.as_ref() {
            nir::ExprKind::Lit(lit) => match lit {
                nir::Lit::Byte(b) => {
                    Ok(Expr::new(ExprKind::Lit(Lit::Byte(*b)), Ty::Byte, expr.span))
                }
                nir::Lit::Int(i) => Ok(Expr::new(ExprKind::Lit(Lit::Int(*i)), Ty::Int, expr.span)),
                nir::Lit::Rational(r) => Ok(Expr::new(
                    ExprKind::Lit(Lit::Rational(*r)),
                    Ty::Rational,
                    expr.span,
                )),
                nir::Lit::Real(r) => {
                    Ok(Expr::new(ExprKind::Lit(Lit::Real(*r)), Ty::Real, expr.span))
                }
                nir::Lit::Bool(b) => {
                    Ok(Expr::new(ExprKind::Lit(Lit::Bool(*b)), Ty::Bool, expr.span))
                }
                nir::Lit::String(s) => Ok(Expr::new(
                    ExprKind::Lit(Lit::String(s.clone())),
                    Ty::String,
                    expr.span,
                )),
                nir::Lit::Char(c) => {
                    Ok(Expr::new(ExprKind::Lit(Lit::Char(*c)), Ty::Char, expr.span))
                }
            },
            nir::ExprKind::Var(ident) => {
                if let Some(scm) = self.ctx.get(&ident.name) {
                    Ok(Expr::new(
                        ExprKind::Var(*ident),
                        scm.instantiate(&mut self.meta_ctx),
                        expr.span,
                    ))
                } else {
                    log::debug!("unbound variable: {:?}", expr);
                    Err(TypeError::from(format!(
                        "unbound variable: {:?} - \"{}\"",
                        expr,
                        self.src[expr.span].to_string()
                    )))
                }
            }
            nir::ExprKind::Apply(fun, arg) => {
                let solved_fun = self.generate_expr_constraints(src, fun)?;
                let solved_arg = self.generate_expr_constraints(src, arg)?;

                let ty_ret = Ty::MetaRef(self.meta_ctx.fresh());
                let ty_fun = Ty::Lambda(Box::new(solved_arg.clone().ty), Box::new(ty_ret.clone()));
                self.constraints
                    .push(Constraint::Eq(solved_fun.ty.clone(), ty_fun));

                Ok(Expr::new(
                    ExprKind::Apply(solved_fun, solved_arg),
                    ty_ret,
                    expr.span,
                ))
            }
            nir::ExprKind::Lambda(pattern, expr) => {
                let param_ty = Ty::MetaRef(self.meta_ctx.fresh());
                log::debug!("fresh lambda var: {:?}", param_ty);
                self.ctx.push();
                let solved_pat =
                    self.generate_pattern_constraints(src, pattern, &param_ty, false)?;
                let solved_expr = self.generate_expr_constraints(src, expr)?;
                let fun_ty = Ty::Lambda(Box::new(param_ty), Box::new(solved_expr.ty.clone()));
                self.ctx.pop();

                Ok(Expr::new(
                    ExprKind::Lambda(solved_pat, solved_expr),
                    fun_ty,
                    expr.span,
                ))
            }
            nir::ExprKind::Or(lhs, rhs) => {
                let solved_lhs = self.generate_expr_constraints(src, lhs)?;
                let solved_rhs = self.generate_expr_constraints(src, rhs)?;

                self.constraints
                    .push(Constraint::Eq(solved_lhs.ty.clone(), Ty::Bool));
                self.constraints
                    .push(Constraint::Eq(solved_rhs.ty.clone(), Ty::Bool));

                Ok(Expr::new(
                    ExprKind::Or(solved_lhs, solved_rhs),
                    Ty::Bool,
                    expr.span,
                ))
            }
            nir::ExprKind::And(lhs, rhs) => {
                let solved_lhs = self.generate_expr_constraints(src, lhs)?;
                let solved_rhs = self.generate_expr_constraints(src, rhs)?;

                self.constraints
                    .push(Constraint::Eq(solved_lhs.ty.clone(), Ty::Bool));
                self.constraints
                    .push(Constraint::Eq(solved_rhs.ty.clone(), Ty::Bool));

                Ok(Expr::new(
                    ExprKind::And(solved_lhs, solved_rhs),
                    Ty::Bool,
                    expr.span,
                ))
            }
            nir::ExprKind::Let(pattern, true, expr, body) => {
                let var = Ty::MetaRef(self.meta_ctx.fresh());
                log::debug!("fresh let var: {:?}", var);

                self.ctx.push();
                let solved_pat = self.generate_pattern_constraints(src, pattern, &var, false)?;
                let solved_expr = self.generate_expr_constraints(src, expr)?;
                let solved_body = self.generate_expr_constraints(src, body)?;
                self.ctx.pop();

                self.constraints.push(Constraint::Eq(
                    solved_pat.ty.clone(),
                    solved_expr.ty.clone(),
                ));

                Ok(Expr::new(
                    ExprKind::Let(solved_pat, true, solved_expr, solved_body.clone()),
                    solved_body.ty,
                    expr.span,
                ))
            }
            nir::ExprKind::Let(pattern, false, expr, body) => {
                let solved_expr = self.generate_expr_constraints(src, expr)?;
                let solved_pat =
                    self.generate_pattern_constraints(src, pattern, &solved_expr.ty, false)?;
                let solved_body = self.generate_expr_constraints(src, body)?;
                self.constraints.push(Constraint::Eq(
                    solved_pat.ty.clone(),
                    solved_expr.ty.clone(),
                ));

                Ok(Expr::new(
                    ExprKind::Let(solved_pat, false, solved_expr, solved_body.clone()),
                    solved_body.ty,
                    expr.span,
                ))
            }
            nir::ExprKind::If(cond, then, else_) => {
                let solved_cond = self.generate_expr_constraints(src, cond)?;
                let solved_then = self.generate_expr_constraints(src, then)?;
                let solved_else = self.generate_expr_constraints(src, else_)?;

                self.constraints
                    .push(Constraint::Eq(solved_cond.ty.clone(), Ty::Bool));
                self.constraints.push(Constraint::Eq(
                    solved_then.ty.clone(),
                    solved_else.ty.clone(),
                ));

                Ok(Expr::new(
                    ExprKind::If(solved_cond, solved_then.clone(), solved_else),
                    solved_then.ty,
                    expr.span,
                ))
            }
            nir::ExprKind::Match(expr, arms) => {
                let solved_expr = self.generate_expr_constraints(src, expr)?;
                let ty = Ty::MetaRef(self.meta_ctx.fresh());
                log::debug!("fresh match var: {:?}", ty);

                let mut solved_arms = vec![];
                for (pat, body) in arms {
                    self.ctx.push();

                    let solved_pat =
                        self.generate_pattern_constraints(src, pat, &solved_expr.ty, false)?;
                    let solved_body = self.generate_expr_constraints(src, body)?;

                    self.constraints.push(Constraint::Eq(
                        solved_pat.ty.clone(),
                        solved_expr.ty.clone(),
                    ));
                    self.constraints
                        .push(Constraint::Eq(solved_body.ty.clone(), ty.clone()));

                    solved_arms.push((solved_pat, solved_body));
                    self.ctx.pop();
                }

                Ok(Expr::new(
                    ExprKind::Match(solved_expr.clone(), solved_arms),
                    ty,
                    expr.span,
                ))
            }
            nir::ExprKind::List(vec) => {
                let elem_ty = Ty::MetaRef(self.meta_ctx.fresh());
                log::debug!("fresh list var: {:?}", elem_ty);
                let list_ty = Ty::List(Box::new(elem_ty.clone()));
                let solved_vec = vec
                    .iter()
                    .map(|expr| self.generate_expr_constraints(src, expr))
                    .collect::<InferResult<Vec<Expr>>>()?;
                for expr in &solved_vec {
                    self.constraints
                        .push(Constraint::Eq(expr.ty.clone(), elem_ty.clone()));
                }

                Ok(Expr::new(ExprKind::List(solved_vec), list_ty, expr.span))
            }
            nir::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, Ty::Unit, expr.span)),
        }
    }

    fn generate_pattern_constraints<'src>(
        &mut self,
        src: &'src str,
        pat: &nir::Pattern,
        ty: &Ty,
        generalize: bool,
    ) -> InferResult<Pattern> {
        match pat.kind.as_ref() {
            nir::PatternKind::Wildcard => {
                Ok(Pattern::new(PatternKind::Wildcard, ty.clone(), pat.span))
            }
            nir::PatternKind::Lit(lit) => Ok(match lit {
                nir::Lit::Byte(b) => {
                    Pattern::new(PatternKind::Lit(Lit::Byte(*b)), Ty::Byte, pat.span)
                }
                nir::Lit::Int(i) => Pattern::new(PatternKind::Lit(Lit::Int(*i)), Ty::Int, pat.span),
                nir::Lit::Rational(r) => {
                    Pattern::new(PatternKind::Lit(Lit::Rational(*r)), Ty::Rational, pat.span)
                }
                nir::Lit::Real(r) => {
                    Pattern::new(PatternKind::Lit(Lit::Real(*r)), Ty::Real, pat.span)
                }
                nir::Lit::Bool(b) => {
                    Pattern::new(PatternKind::Lit(Lit::Bool(*b)), Ty::Bool, pat.span)
                }
                nir::Lit::String(s) => Pattern::new(
                    PatternKind::Lit(Lit::String(s.clone())),
                    Ty::String,
                    pat.span,
                ),
                nir::Lit::Char(c) => {
                    Pattern::new(PatternKind::Lit(Lit::Char(*c)), Ty::Char, pat.span)
                }
            }),
            nir::PatternKind::Ident(ident, type_hint) => {
                if generalize {
                    let scm = ty.generalize(&self.ctx, &self.meta_ctx);
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
            nir::PatternKind::List(vec) => {
                let v = self.meta_ctx.fresh();
                let elem_ty = Ty::MetaRef(v);
                log::debug!(
                    "fresh list pat var: {:?} is {:?}",
                    elem_ty,
                    self.meta_ctx.get(&v)
                );
                let list_ty = Ty::List(Box::new(elem_ty.clone()));
                let solved_pats = vec
                    .iter()
                    .map(|pat| self.generate_pattern_constraints(src, pat, &elem_ty, generalize))
                    .collect::<InferResult<Vec<Pattern>>>()?;
                self.constraints
                    .push(Constraint::Eq(ty.clone(), list_ty.clone()));
                Ok(Pattern::new(
                    PatternKind::List(solved_pats),
                    list_ty,
                    pat.span,
                ))
            }
            nir::PatternKind::Pair(head, tail) => {
                let var = Ty::MetaRef(self.meta_ctx.fresh());
                log::debug!("fresh pair pat var: {:?}", var);
                let pair_ty = Ty::List(Box::new(var.clone()));
                let solved_head = self.generate_pattern_constraints(src, head, &var, generalize)?;
                let solved_tail =
                    self.generate_pattern_constraints(src, tail, &pair_ty, generalize)?;
                self.constraints
                    .push(Constraint::Eq(ty.clone(), pair_ty.clone()));
                Ok(Pattern::new(
                    PatternKind::Pair(solved_head, solved_tail),
                    pair_ty,
                    pat.span,
                ))
            }
            nir::PatternKind::Unit => Ok(Pattern::new(PatternKind::Unit, Ty::Unit, pat.span)),
        }
    }
}
