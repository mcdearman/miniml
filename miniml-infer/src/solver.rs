use itertools::Itertools;
use meta::MetaId;
use miniml_ast::SynNode;
use miniml_nir as nir;
use miniml_tir::{meta::Meta, scheme::Scheme, ty::Ty, *};
use miniml_utils::intern::InternedString;

use crate::{
    context::Context,
    error::{InferResult, TypeError},
};

#[derive(Debug, Clone)]
pub struct TypeSolver {
    src: InternedString,
    pub ctx: Context,
}

impl TypeSolver {
    pub fn new() -> Self {
        let mut ctx = Context::new();

        ctx.insert(
            "main".into(),
            Scheme::new(
                0,
                Ty::Arrow(Box::new(Ty::Array(Box::new(Ty::String))), Box::new(Ty::Int)),
            ),
        );

        ctx.insert(
            "__neg__".into(),
            Scheme::new(0, Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Int))),
        );

        ctx.insert(
            "__not__".into(),
            Scheme::new(0, Ty::Arrow(Box::new(Ty::Bool), Box::new(Ty::Bool))),
        );

        ctx.insert(
            "__add__".into(),
            Scheme::new(
                0,
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__sub__".into(),
            Scheme::new(
                0,
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__mul__".into(),
            Scheme::new(
                0,
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__div__".into(),
            Scheme::new(
                0,
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__rem__".into(),
            Scheme::new(
                0,
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__pow__".into(),
            Scheme::new(
                0,
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__eq__".into(),
            Scheme::new(
                0,
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        ctx.insert(
            "__neq__".into(),
            Scheme::new(
                0,
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        ctx.insert(
            "__lt__".into(),
            Scheme::new(
                0,
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        ctx.insert(
            "__lte__".into(),
            Scheme::new(
                0,
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        ctx.insert(
            "__gt__".into(),
            Scheme::new(
                0,
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        ctx.insert(
            "__gte__".into(),
            Scheme::new(
                0,
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        let r = MetaId::fresh();
        ctx.insert(
            "__pair__".into(),
            Ty::Arrow(
                Box::new(Ty::Meta(r)),
                Box::new(Ty::Arrow(
                    Box::new(Ty::List(Box::new(Ty::Meta(r)))),
                    Box::new(Ty::List(Box::new(Ty::Meta(r)))),
                )),
            )
            .generalize(ctx.free_vars(), true),
        );

        Self {
            src: "".into(),
            ctx,
        }
    }

    pub fn infer<'src>(
        &mut self,
        src: &'src str,
        nir: &nir::Prog,
    ) -> (Option<Prog>, Vec<TypeError>) {
        let (prog, errors) = self.infer_prog(src, nir);
        (prog.map(|m| self.zonk(m)), errors)
    }

    pub fn unify(&mut self, t1: &Ty, t2: &Ty) -> InferResult<()> {
        log::debug!("unify: {:?} and {:?}", t1, t2);
        let t1 = t1.force();
        let t2 = t2.force();

        match (&t1, &t2) {
            (Ty::Byte, Ty::Byte)
            | (Ty::Int, Ty::Int)
            | (Ty::Rational, Ty::Rational)
            | (Ty::Real, Ty::Real)
            | (Ty::Bool, Ty::Bool)
            | (Ty::String, Ty::String)
            | (Ty::Char, Ty::Char)
            | (Ty::Unit, Ty::Unit) => Ok(()),
            (Ty::Arrow(p1, b1), Ty::Arrow(p2, b2)) => {
                self.unify(p1, p2)?;
                self.unify(b1, b2)
            }
            (Ty::List(l1), Ty::List(l2)) => self.unify(l1, l2),
            (_, Ty::Meta(meta_ref)) => {
                self.bind(meta_ref, &t1)?;
                log::debug!("bind: {:?} to {:?}", meta_ref, t1);
                // log::debug!("meta_ctx: {:#?}", self);
                Ok(())
            }
            (Ty::Meta(meta_ref), _) => {
                self.bind(meta_ref, &t2)?;
                log::debug!("bind: {:?} to {:?}", meta_ref, t2);
                Ok(())
            }
            _ => Err(TypeError::UnificationMismatch(t1, t2)),
        }
    }

    pub fn bind(&mut self, var_id: &MetaId, ty: &Ty) -> InferResult<()> {
        match var_id.get() {
            Meta::Bound(t) => self.unify(&t, ty),
            Meta::Unbound(id) => {
                if *ty == Ty::Meta(*var_id) {
                    Ok(())
                } else if ty.free_vars().contains(&id) {
                    Err(TypeError::OccursCheck(ty.clone(), Ty::Meta(*var_id)))
                } else {
                    var_id.insert(Meta::Bound(ty.clone()));
                    Ok(())
                }
            }
        }
    }

    fn infer_prog<'src>(
        &mut self,
        src: &'src str,
        nir: &nir::Prog,
    ) -> (Option<Prog>, Vec<TypeError>) {
        let mut decls = vec![];
        let mut errors = vec![];

        for decl in &nir.inner.decls {
            match self.infer_def(src, decl) {
                Ok(decl) => {
                    decls.push(decl);
                }
                Err(e) => errors.push(e),
            }
        }

        (
            Some(SynNode::new(
                Module {
                    name: nir.inner.name.clone(),
                    imports: nir.inner.imports.clone(),
                    decls,
                },
                nir.meta,
            )),
            errors,
        )
    }

    fn infer_def<'src>(&mut self, src: &'src str, decl: &nir::Decl) -> InferResult<Decl> {
        match &decl.inner {
            nir::DeclKind::Def(def) => match &def.inner {
                nir::DefKind::Rec { ident, anno, body } => {
                    if let Some(hint) = anno {
                        todo!()
                    } else {
                        let var = Ty::Meta(MetaId::fresh());
                        log::debug!("fresh rec def var: {:?}", var);

                        self.ctx.push();
                        self.ctx
                            .insert(ident.inner.name, Scheme::new(0, var.clone()));
                        let solved_body = self.infer_expr(src, body)?;
                        self.ctx.pop();

                        self.unify(&var, &solved_body.inner.ty)?;

                        let scm = solved_body.inner.ty.generalize(self.ctx.free_vars(), true);
                        self.ctx.insert(ident.inner.name.clone(), scm.clone());

                        Ok(Decl::new(
                            DeclKind::Def(Def::new(
                                Typed::new(
                                    DefKind::Rec {
                                        ident: ident.clone(),
                                        body: solved_body.clone(),
                                    },
                                    var,
                                ),
                                decl.meta,
                            )),
                            decl.meta,
                        ))
                    }
                }
                nir::DefKind::NonRec { pat, body } => {
                    let solved_expr = self.infer_expr(src, body)?;
                    let solved_pat = self.infer_pattern(src, pat, &solved_expr.inner.ty, true)?;

                    Ok(Decl::new(
                        DeclKind::Def(Def::new(
                            Typed::new(
                                DefKind::NonRec {
                                    pat: solved_pat,
                                    body: solved_expr.clone(),
                                },
                                solved_expr.inner.ty.clone(),
                            ),
                            decl.meta,
                        )),
                        decl.meta,
                    ))
                }
            },
            nir::DeclKind::DefGroup(defs) => {
                todo!()
            }
        }
    }

    fn infer_expr<'src>(&mut self, src: &'src str, expr: &nir::Expr) -> InferResult<Expr> {
        match expr.inner.as_ref() {
            nir::ExprKind::Lit(lit) => match lit {
                nir::Lit::Byte(b) => Ok(Expr::new(
                    Typed::new(ExprKind::Lit(Lit::Byte(*b)), Ty::Byte),
                    expr.meta,
                )),
                nir::Lit::Int(i) => Ok(Expr::new(
                    Typed::new(ExprKind::Lit(Lit::Int(*i)), Ty::Int),
                    expr.meta,
                )),
                nir::Lit::Rational(r) => Ok(Expr::new(
                    Typed::new(ExprKind::Lit(Lit::Rational(*r)), Ty::Rational),
                    expr.meta,
                )),
                nir::Lit::Real(r) => Ok(Expr::new(
                    Typed::new(ExprKind::Lit(Lit::Real(*r)), Ty::Real),
                    expr.meta,
                )),
                nir::Lit::Bool(b) => Ok(Expr::new(
                    Typed::new(ExprKind::Lit(Lit::Bool(*b)), Ty::Bool),
                    expr.meta,
                )),
                nir::Lit::String(s) => Ok(Expr::new(
                    Typed::new(ExprKind::Lit(Lit::String(s.clone())), Ty::String),
                    expr.meta,
                )),
                nir::Lit::Char(c) => Ok(Expr::new(
                    Typed::new(ExprKind::Lit(Lit::Char(*c)), Ty::Char),
                    expr.meta,
                )),
            },
            nir::ExprKind::Var(ident) => {
                if let Some(scm) = self.ctx.get(&ident.inner.name) {
                    Ok(Expr::new(
                        Typed::new(ExprKind::Var(*ident), scm.instantiate()),
                        expr.meta,
                    ))
                } else {
                    log::debug!("unbound variable: {:?}", expr);
                    // Err(TypeError::from(format!(
                    //     "unbound variable: {:?} - \"{}\"",
                    //     expr,
                    //     self.src[expr.meta].to_string()
                    // )))
                    Err(TypeError::UnboundName(ident.inner.name.clone()))
                }
            }
            nir::ExprKind::Apply(fun, arg) => {
                let solved_fun = self.infer_expr(src, fun)?;
                let solved_arg = self.infer_expr(src, arg)?;

                let ty_ret = Ty::Meta(MetaId::fresh());
                let ty_fun = Ty::Arrow(
                    Box::new(solved_arg.inner.ty.clone()),
                    Box::new(ty_ret.clone()),
                );

                self.unify(&solved_fun.inner.ty, &ty_fun)?;

                Ok(Expr::new(
                    Typed::new(ExprKind::Apply(solved_fun, solved_arg), ty_ret),
                    expr.meta,
                ))
            }
            nir::ExprKind::Lambda(pattern, body) => {
                let param_ty = Ty::Meta(MetaId::fresh());
                log::debug!("fresh lambda var: {:?}", param_ty);
                self.ctx.push();
                let solved_pat = self.infer_pattern(src, pattern, &param_ty, false)?;
                let solved_body = self.infer_expr(src, body)?;
                let fun_ty = Ty::Arrow(Box::new(param_ty), Box::new(solved_body.inner.ty.clone()));
                self.ctx.pop();

                Ok(Expr::new(
                    Typed::new(ExprKind::Lambda(solved_pat, solved_body), fun_ty),
                    expr.meta,
                ))
            }
            nir::ExprKind::Or(lhs, rhs) => {
                let solved_lhs = self.infer_expr(src, lhs)?;
                let solved_rhs = self.infer_expr(src, rhs)?;

                self.unify(&solved_lhs.inner.ty, &Ty::Bool)?;
                self.unify(&solved_rhs.inner.ty, &Ty::Bool)?;

                Ok(Expr::new(
                    Typed::new(ExprKind::Or(solved_lhs, solved_rhs), Ty::Bool),
                    expr.meta,
                ))
            }
            nir::ExprKind::And(lhs, rhs) => {
                let solved_lhs = self.infer_expr(src, lhs)?;
                let solved_rhs = self.infer_expr(src, rhs)?;

                self.unify(&solved_lhs.inner.ty, &Ty::Bool)?;
                self.unify(&solved_rhs.inner.ty, &Ty::Bool)?;

                Ok(Expr::new(
                    Typed::new(ExprKind::And(solved_lhs, solved_rhs), Ty::Bool),
                    expr.meta,
                ))
            }
            nir::ExprKind::Let(pattern, true, expr, body) => {
                let var = Ty::Meta(MetaId::fresh());
                log::debug!("fresh let var: {:?}", var);

                self.ctx.push();
                let solved_pat = self.infer_pattern(src, pattern, &var, false)?;
                let solved_expr = self.infer_expr(src, expr)?;
                let solved_body = self.infer_expr(src, body)?;
                self.ctx.pop();

                Ok(Expr::new(
                    Typed::new(
                        ExprKind::Let(solved_pat, true, solved_expr, solved_body.clone()),
                        solved_body.inner.ty,
                    ),
                    expr.meta,
                ))
            }
            nir::ExprKind::Let(pattern, false, expr, body) => {
                let solved_expr = self.infer_expr(src, expr)?;
                let solved_pat = self.infer_pattern(src, pattern, &solved_expr.inner.ty, false)?;
                let solved_body = self.infer_expr(src, body)?;

                Ok(Expr::new(
                    Typed::new(
                        ExprKind::Let(solved_pat, false, solved_expr, solved_body.clone()),
                        solved_body.inner.ty,
                    ),
                    expr.meta,
                ))
            }
            nir::ExprKind::If(cond, then, else_) => {
                let solved_cond = self.infer_expr(src, cond)?;
                let solved_then = self.infer_expr(src, then)?;
                let solved_else = self.infer_expr(src, else_)?;

                self.unify(&solved_cond.inner.ty, &Ty::Bool)?;
                self.unify(&solved_then.inner.ty, &solved_else.inner.ty)?;

                Ok(Expr::new(
                    Typed::new(
                        ExprKind::If(solved_cond, solved_then.clone(), solved_else),
                        solved_then.inner.ty,
                    ),
                    expr.meta,
                ))
            }
            nir::ExprKind::Match(e, arms) => {
                let solved_expr = self.infer_expr(src, &e)?;
                let ty = Ty::Meta(MetaId::fresh());
                log::debug!("fresh match var: {:?}", ty);

                let mut solved_arms = vec![];
                for (pat, body) in arms {
                    self.ctx.push();

                    let solved_pat = self.infer_pattern(src, pat, &solved_expr.inner.ty, false)?;
                    let solved_body = self.infer_expr(src, body)?;

                    self.unify(&solved_body.inner.ty, &ty)?;

                    solved_arms.push((solved_pat, solved_body));
                    self.ctx.pop();
                }

                Ok(Expr::new(
                    Typed::new(ExprKind::Match(solved_expr.clone(), solved_arms), ty),
                    expr.meta,
                ))
            }
            nir::ExprKind::List(vec) => {
                let elem_ty = Ty::Meta(MetaId::fresh());
                log::debug!("fresh list var: {:?}", elem_ty);
                let list_ty = Ty::List(Box::new(elem_ty.clone()));
                let solved_vec = vec
                    .iter()
                    .map(|expr| self.infer_expr(src, expr))
                    .collect::<InferResult<Vec<Expr>>>()?;
                for expr in &solved_vec {
                    self.unify(&expr.inner.ty, &elem_ty)?;
                }

                Ok(Expr::new(
                    Typed::new(ExprKind::List(solved_vec), list_ty),
                    expr.meta,
                ))
            }
            nir::ExprKind::Unit => Ok(Expr::new(Typed::new(ExprKind::Unit, Ty::Unit), expr.meta)),
        }
    }

    fn infer_pattern<'src>(
        &mut self,
        src: &'src str,
        pat: &nir::Pattern,
        ty: &Ty,
        generalize: bool,
    ) -> InferResult<Pattern> {
        match pat.inner.as_ref() {
            nir::PatternKind::Wildcard => Ok(Pattern::new(
                Typed::new(PatternKind::Wildcard, ty.clone()),
                pat.meta,
            )),
            nir::PatternKind::Lit(lit) => Ok(match lit {
                nir::Lit::Byte(b) => Pattern::new(
                    Typed::new(PatternKind::Lit(Lit::Byte(*b)), Ty::Byte),
                    pat.meta,
                ),
                nir::Lit::Int(i) => Pattern::new(
                    Typed::new(PatternKind::Lit(Lit::Int(*i)), Ty::Int),
                    pat.meta,
                ),
                nir::Lit::Rational(r) => Pattern::new(
                    Typed::new(PatternKind::Lit(Lit::Rational(*r)), Ty::Rational),
                    pat.meta,
                ),
                nir::Lit::Real(r) => Pattern::new(
                    Typed::new(PatternKind::Lit(Lit::Real(*r)), Ty::Real),
                    pat.meta,
                ),
                nir::Lit::Bool(b) => Pattern::new(
                    Typed::new(PatternKind::Lit(Lit::Bool(*b)), Ty::Bool),
                    pat.meta,
                ),
                nir::Lit::String(s) => Pattern::new(
                    Typed::new(PatternKind::Lit(Lit::String(s.clone())), Ty::String),
                    pat.meta,
                ),
                nir::Lit::Char(c) => Pattern::new(
                    Typed::new(PatternKind::Lit(Lit::Char(*c)), Ty::Char),
                    pat.meta,
                ),
            }),
            nir::PatternKind::Ident(ident, type_hint) => {
                if generalize {
                    let scm = ty.generalize(self.ctx.free_vars(), false);
                    self.ctx.insert(ident.inner.name, scm.clone());
                    Ok(Pattern::new(
                        Typed::new(PatternKind::Ident(*ident), ty.clone()),
                        pat.meta,
                    ))
                } else {
                    self.ctx
                        .insert(ident.inner.name, Scheme::new(0, ty.clone()));
                    Ok(Pattern::new(
                        Typed::new(PatternKind::Ident(*ident), ty.clone()),
                        pat.meta,
                    ))
                }
            }
            nir::PatternKind::List(vec) => {
                let v = MetaId::fresh();
                let elem_ty = Ty::Meta(v);
                log::debug!("fresh list pat var: {:?} is {:?}", elem_ty, v.get());

                let list_ty = Ty::List(Box::new(elem_ty.clone()));
                let solved_pats = vec
                    .iter()
                    .map(|pat| self.infer_pattern(src, pat, &elem_ty, generalize))
                    .collect::<InferResult<Vec<Pattern>>>()?;

                self.unify(ty, &list_ty)?;

                Ok(Pattern::new(
                    Typed::new(PatternKind::List(solved_pats), list_ty),
                    pat.meta,
                ))
            }
            nir::PatternKind::Pair(head, tail) => {
                let var = Ty::Meta(MetaId::fresh());
                log::debug!("fresh pair pat var: {:?}", var);

                let pair_ty = Ty::List(Box::new(var.clone()));
                let solved_head = self.infer_pattern(src, head, &var, generalize)?;
                let solved_tail = self.infer_pattern(src, tail, &pair_ty, generalize)?;

                self.unify(ty, &pair_ty)?;

                Ok(Pattern::new(
                    Typed::new(PatternKind::Pair(solved_head, solved_tail), pair_ty),
                    pat.meta,
                ))
            }
            nir::PatternKind::Unit => Ok(Pattern::new(
                Typed::new(PatternKind::Unit, Ty::Unit),
                pat.meta,
            )),
        }
    }

    fn zonk(&mut self, prog: Prog) -> Prog {
        prog.map(|m| {
            let decls = m.decls.iter().map(|d| self.zonk_decl(d)).collect();
            Module {
                name: m.name,
                imports: m.imports,
                decls,
            }
        })
    }

    fn zonk_decl(&mut self, decl: &Decl) -> Decl {
        match &decl.inner {
            DeclKind::Def(def) => decl.map_ref(|_| DeclKind::Def(self.zonk_def(def))),
            DeclKind::DefGroup(defs) => decl.map_ref(|_| {
                DeclKind::DefGroup(defs.iter().map(|def| self.zonk_def(def)).collect_vec())
            }),
        }
    }

    fn zonk_def(&mut self, def: &Def) -> Def {
        match &def.inner.value {
            DefKind::Rec { ident, body } => Def::new(
                Typed::new(
                    DefKind::Rec {
                        ident: ident.clone(),
                        body: self.zonk_expr(body),
                    },
                    def.inner.ty.zonk(),
                ),
                def.meta,
            ),
            DefKind::NonRec { pat, body } => Def::new(
                Typed::new(
                    DefKind::NonRec {
                        pat: self.zonk_pattern(pat),
                        body: self.zonk_expr(body),
                    },
                    def.inner.ty.zonk(),
                ),
                def.meta,
            ),
        }
    }

    fn zonk_expr(&mut self, expr: &Expr) -> Expr {
        match &expr.inner.value {
            ExprKind::Lit(lit) => Expr::new(
                Typed::new(ExprKind::Lit(lit.clone()), expr.inner.ty.zonk()),
                expr.meta,
            ),
            ExprKind::Var(node) => Expr::new(
                Typed::new(ExprKind::Var(node.clone()), expr.inner.ty.zonk()),
                expr.meta,
            ),
            ExprKind::Apply(fun, arg) => Expr::new(
                Typed::new(
                    ExprKind::Apply(self.zonk_expr(fun), self.zonk_expr(arg)),
                    expr.inner.ty.zonk(),
                ),
                expr.meta,
            ),
            ExprKind::Lambda(param, expr) => Expr::new(
                Typed::new(
                    ExprKind::Lambda(self.zonk_pattern(param), self.zonk_expr(expr)),
                    expr.inner.ty.zonk(),
                ),
                expr.meta,
            ),
            ExprKind::Or(lhs, rhs) => Expr::new(
                Typed::new(
                    ExprKind::Or(self.zonk_expr(lhs), self.zonk_expr(rhs)),
                    expr.inner.ty.zonk(),
                ),
                expr.meta,
            ),
            ExprKind::And(lhs, rhs) => Expr::new(
                Typed::new(
                    ExprKind::And(self.zonk_expr(lhs), self.zonk_expr(rhs)),
                    expr.inner.ty.zonk(),
                ),
                expr.meta,
            ),
            ExprKind::Let(pat, _, expr, body) => Expr::new(
                Typed::new(
                    ExprKind::Let(
                        self.zonk_pattern(pat),
                        true,
                        self.zonk_expr(expr),
                        self.zonk_expr(body),
                    ),
                    expr.inner.ty.zonk(),
                ),
                expr.meta,
            ),
            ExprKind::If(cond, then, else_) => Expr::new(
                Typed::new(
                    ExprKind::If(
                        self.zonk_expr(cond),
                        self.zonk_expr(then),
                        self.zonk_expr(else_),
                    ),
                    expr.inner.ty.zonk(),
                ),
                expr.meta,
            ),
            ExprKind::Match(expr, cases) => Expr::new(
                Typed::new(
                    ExprKind::Match(
                        self.zonk_expr(expr),
                        cases
                            .iter()
                            .map(|(pat, body)| (self.zonk_pattern(pat), self.zonk_expr(body)))
                            .collect_vec(),
                    ),
                    expr.inner.ty.zonk(),
                ),
                expr.meta,
            ),
            ExprKind::List(xs) => Expr::new(
                Typed::new(
                    ExprKind::List(xs.iter().map(|x| self.zonk_expr(x)).collect_vec()),
                    expr.inner.ty.zonk(),
                ),
                expr.meta,
            ),
            ExprKind::Unit => Expr::new(Typed::new(ExprKind::Unit, Ty::Unit), expr.meta),
        }
    }

    fn zonk_pattern(&mut self, pat: &Pattern) -> Pattern {
        match &pat.inner.value {
            PatternKind::Wildcard => Pattern::new(
                Typed::new(PatternKind::Wildcard, pat.inner.ty.zonk()),
                pat.meta,
            ),
            PatternKind::Lit(lit) => Pattern::new(
                Typed::new(PatternKind::Lit(lit.clone()), pat.inner.ty.zonk()),
                pat.meta,
            ),
            PatternKind::Ident(ident) => Pattern::new(
                Typed::new(PatternKind::Ident(ident.clone()), pat.inner.ty.zonk()),
                pat.meta,
            ),
            PatternKind::List(xs) => Pattern::new(
                Typed::new(
                    PatternKind::List(xs.iter().map(|x| self.zonk_pattern(x)).collect_vec()),
                    pat.inner.ty.zonk(),
                ),
                pat.meta,
            ),
            PatternKind::Pair(head, tail) => Pattern::new(
                Typed::new(
                    PatternKind::Pair(self.zonk_pattern(head), self.zonk_pattern(tail)),
                    pat.inner.ty.zonk(),
                ),
                pat.meta,
            ),
            PatternKind::Unit => {
                Pattern::new(Typed::new(PatternKind::Unit, pat.inner.ty.zonk()), pat.meta)
            }
        }
    }
}
