use itertools::Itertools;
use miniml_ast::SynNode;
use miniml_nir as nir;
use miniml_tir::{scheme::Scheme, ty::Ty, ty_var::TyVar, *};
use miniml_utils::intern::InternedString;
use ty_var::VarId;

use crate::{
    constraint::Constraint,
    context::Context,
    error::{InferResult, TypeError},
};

#[derive(Debug, Clone)]
pub struct TypeSolver {
    src: InternedString,
    pub ctx: Context,
    constraints: Vec<Constraint>,
}

impl TypeSolver {
    pub fn new() -> Self {
        let mut ctx = Context::new();

        ctx.insert(
            "main".into(),
            Scheme::new(
                vec![],
                Ty::Arrow(Box::new(Ty::Array(Box::new(Ty::String))), Box::new(Ty::Int)),
            ),
        );

        ctx.insert(
            "__neg__".into(),
            Scheme::new(vec![], Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Int))),
        );

        ctx.insert(
            "__not__".into(),
            Scheme::new(vec![], Ty::Arrow(Box::new(Ty::Bool), Box::new(Ty::Bool))),
        );

        ctx.insert(
            "__add__".into(),
            Scheme::new(
                vec![],
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__sub__".into(),
            Scheme::new(
                vec![],
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__mul__".into(),
            Scheme::new(
                vec![],
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__div__".into(),
            Scheme::new(
                vec![],
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__rem__".into(),
            Scheme::new(
                vec![],
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__pow__".into(),
            Scheme::new(
                vec![],
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Int))),
                ),
            ),
        );

        ctx.insert(
            "__eq__".into(),
            Scheme::new(
                vec![],
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        ctx.insert(
            "__neq__".into(),
            Scheme::new(
                vec![],
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        ctx.insert(
            "__lt__".into(),
            Scheme::new(
                vec![],
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        ctx.insert(
            "__lte__".into(),
            Scheme::new(
                vec![],
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        ctx.insert(
            "__gt__".into(),
            Scheme::new(
                vec![],
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        ctx.insert(
            "__gte__".into(),
            Scheme::new(
                vec![],
                Ty::Arrow(
                    Box::new(Ty::Int),
                    Box::new(Ty::Arrow(Box::new(Ty::Int), Box::new(Ty::Bool))),
                ),
            ),
        );

        let r = VarId::fresh();
        ctx.insert(
            "__pair__".into(),
            Ty::Arrow(
                Box::new(Ty::Var(r)),
                Box::new(Ty::Arrow(
                    Box::new(Ty::List(Box::new(Ty::Var(r)))),
                    Box::new(Ty::List(Box::new(Ty::Var(r)))),
                )),
            )
            .generalize(ctx.free_vars()),
        );

        Self {
            src: "".into(),
            ctx,
            constraints: vec![],
        }
    }

    pub fn infer<'src>(
        &mut self,
        src: &'src str,
        nir: &nir::Prog,
    ) -> (Option<Prog>, Vec<TypeError>) {
        let (prog, gen_errors) = self.generate_prog_constraints(src, nir);
        // log::debug!("prog: {:#?}", prog);
        log::debug!("constraints: {:#?}", self.constraints);
        let unify_errors = self.solve_constraints();
        let errors = gen_errors.into_iter().chain(unify_errors).collect_vec();
        self.constraints.clear();
        (prog.map(|m| self.zonk(m)), errors)
    }

    fn solve_constraints<'src>(&mut self) -> Vec<TypeError> {
        let mut errors = vec![];
        for c in self.constraints.clone() {
            match &c {
                Constraint::Eq(t1, t2) => {
                    if let Err(e) = self.unify(t1, t2) {
                        errors.push(e);
                    }
                }
                Constraint::Gen(name, t) => {
                    let p = t.generalize(self.ctx.free_vars());
                    self.ctx.insert(name.clone(), p);
                }
            }
        }
        errors
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
            (_, Ty::Var(meta_ref)) => {
                self.bind(meta_ref, &t1)?;
                log::debug!("bind: {:?} to {:?}", meta_ref, t1);
                // log::debug!("meta_ctx: {:#?}", self);
                Ok(())
            }
            (Ty::Var(meta_ref), _) => {
                self.bind(meta_ref, &t2)?;
                log::debug!("bind: {:?} to {:?}", meta_ref, t2);
                // log::debug!("meta_ctx: {:#?}", self);
                Ok(())
            }
            // (Type::Poly(p1), Type::Poly(p2)) => todo!(),
            // (Type::Poly(p), _) => {
            //     let p = p.instantiate(self);
            //     self.unify(&p, &t2)
            // }
            // (_, Type::Poly(p)) => {
            //     let p = p.instantiate(self);
            //     self.unify(&t1, &p)
            // }
            _ => Err(TypeError::UnificationMismatch(t1, t2)),
        }
    }

    pub fn bind(&mut self, var_id: &VarId, ty: &Ty) -> InferResult<()> {
        match var_id.get() {
            TyVar::Bound(t) => self.unify(&t, ty),
            TyVar::Unbound(id) => {
                if *ty == Ty::Var(*var_id) {
                    Ok(())
                } else if ty.free_vars().contains(&id) {
                    Err(TypeError::OccursCheck(ty.clone(), Ty::Var(*var_id)))
                } else {
                    var_id.insert(TyVar::Bound(ty.clone()));
                    Ok(())
                }
            }
        }
    }

    fn generate_prog_constraints<'src>(
        &mut self,
        src: &'src str,
        nir: &nir::Prog,
    ) -> (Option<Prog>, Vec<TypeError>) {
        let mut decls = vec![];
        let mut errors = vec![];

        for decl in &nir.value.decls {
            match self.generate_def_constraints(src, decl) {
                Ok(decl) => {
                    decls.push(decl);
                }
                Err(e) => errors.push(e),
            }
        }

        (
            Some(SynNode::new(
                Module {
                    name: nir.value.name.clone(),
                    imports: nir.value.imports.clone(),
                    decls,
                },
                nir.meta,
            )),
            errors,
        )
    }

    fn generate_def_constraints<'src>(
        &mut self,
        src: &'src str,
        decl: &nir::Decl,
    ) -> InferResult<Decl> {
        match &decl.value {
            nir::DeclKind::Def(def) => match &def.value {
                nir::DefKind::Rec { ident, anno, body } => {
                    if let Some(hint) = anno {
                        todo!()
                    } else {
                        let var = Ty::Var(VarId::fresh());
                        log::debug!("fresh rec def var: {:?}", var);

                        self.ctx.push();
                        self.ctx
                            .insert(ident.value.name, Scheme::new(vec![], var.clone()));
                        let solved_body = self.generate_expr_constraints(src, body)?;
                        self.ctx.pop();

                        self.constraints.push(Constraint::Gen(
                            ident.value.name.clone(),
                            solved_body.meta.0.clone(),
                        ));

                        self.constraints
                            .push(Constraint::Eq(var.clone(), solved_body.meta.0.clone()));

                        Ok(Decl::new(
                            DeclKind::Def(Def::new(
                                DefKind::Rec {
                                    ident: ident.clone(),
                                    body: solved_body.clone(),
                                },
                                (solved_body.meta.0.clone(), decl.meta),
                            )),
                            decl.meta,
                        ))
                    }
                }
                nir::DefKind::NonRec { pat, body } => {
                    let solved_expr = self.generate_expr_constraints(src, body)?;
                    let solved_pat =
                        self.generate_pattern_constraints(src, pat, &solved_expr.meta.0, true)?;

                    Ok(Decl::new(
                        DeclKind::Def(Def::new(
                            DefKind::NonRec {
                                pat: solved_pat,
                                body: solved_expr.clone(),
                            },
                            (solved_expr.meta.0.clone(), decl.meta),
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

    fn generate_expr_constraints<'src>(
        &mut self,
        src: &'src str,
        expr: &nir::Expr,
    ) -> InferResult<Expr> {
        match expr.value.as_ref() {
            nir::ExprKind::Lit(lit) => match lit {
                nir::Lit::Byte(b) => Ok(Expr::new(
                    ExprKind::Lit(Lit::Byte(*b)),
                    (Ty::Byte, expr.meta),
                )),
                nir::Lit::Int(i) => {
                    Ok(Expr::new(ExprKind::Lit(Lit::Int(*i)), (Ty::Int, expr.meta)))
                }
                nir::Lit::Rational(r) => Ok(Expr::new(
                    ExprKind::Lit(Lit::Rational(*r)),
                    (Ty::Rational, expr.meta),
                )),
                nir::Lit::Real(r) => Ok(Expr::new(
                    ExprKind::Lit(Lit::Real(*r)),
                    (Ty::Real, expr.meta),
                )),
                nir::Lit::Bool(b) => Ok(Expr::new(
                    ExprKind::Lit(Lit::Bool(*b)),
                    (Ty::Bool, expr.meta),
                )),
                nir::Lit::String(s) => Ok(Expr::new(
                    ExprKind::Lit(Lit::String(s.clone())),
                    (Ty::String, expr.meta),
                )),
                nir::Lit::Char(c) => Ok(Expr::new(
                    ExprKind::Lit(Lit::Char(*c)),
                    (Ty::Char, expr.meta),
                )),
            },
            nir::ExprKind::Var(ident) => {
                if let Some(scm) = self.ctx.get(&ident.value.name) {
                    Ok(Expr::new(
                        ExprKind::Var(*ident),
                        (scm.instantiate(), expr.meta),
                    ))
                } else {
                    log::debug!("unbound variable: {:?}", expr);
                    // Err(TypeError::from(format!(
                    //     "unbound variable: {:?} - \"{}\"",
                    //     expr,
                    //     self.src[expr.meta].to_string()
                    // )))
                    Err(TypeError::UnboundName(ident.value.name.clone()))
                }
            }
            nir::ExprKind::Apply(fun, arg) => {
                let solved_fun = self.generate_expr_constraints(src, fun)?;
                let solved_arg = self.generate_expr_constraints(src, arg)?;

                let ty_ret = Ty::Var(VarId::fresh());
                let ty_fun = Ty::Arrow(
                    Box::new(solved_arg.meta.0.clone()),
                    Box::new(ty_ret.clone()),
                );
                self.constraints
                    .push(Constraint::Eq(solved_fun.meta.0.clone(), ty_fun));

                Ok(Expr::new(
                    ExprKind::Apply(solved_fun, solved_arg),
                    (ty_ret, expr.meta),
                ))
            }
            nir::ExprKind::Lambda(pattern, body) => {
                let param_ty = Ty::Var(VarId::fresh());
                log::debug!("fresh lambda var: {:?}", param_ty);
                self.ctx.push();
                let solved_pat =
                    self.generate_pattern_constraints(src, pattern, &param_ty, false)?;
                let solved_body = self.generate_expr_constraints(src, body)?;
                let fun_ty = Ty::Arrow(Box::new(param_ty), Box::new(solved_body.meta.0.clone()));
                self.ctx.pop();

                Ok(Expr::new(
                    ExprKind::Lambda(solved_pat, solved_body),
                    (fun_ty, expr.meta),
                ))
            }
            nir::ExprKind::Or(lhs, rhs) => {
                let solved_lhs = self.generate_expr_constraints(src, lhs)?;
                let solved_rhs = self.generate_expr_constraints(src, rhs)?;

                self.constraints
                    .push(Constraint::Eq(solved_lhs.meta.0.clone(), Ty::Bool));
                self.constraints
                    .push(Constraint::Eq(solved_rhs.meta.0.clone(), Ty::Bool));

                Ok(Expr::new(
                    ExprKind::Or(solved_lhs, solved_rhs),
                    (Ty::Bool, expr.meta),
                ))
            }
            nir::ExprKind::And(lhs, rhs) => {
                let solved_lhs = self.generate_expr_constraints(src, lhs)?;
                let solved_rhs = self.generate_expr_constraints(src, rhs)?;

                self.constraints
                    .push(Constraint::Eq(solved_lhs.meta.0.clone(), Ty::Bool));
                self.constraints
                    .push(Constraint::Eq(solved_rhs.meta.0.clone(), Ty::Bool));

                Ok(Expr::new(
                    ExprKind::And(solved_lhs, solved_rhs),
                    (Ty::Bool, expr.meta),
                ))
            }
            nir::ExprKind::Let(pattern, true, expr, body) => {
                let var = Ty::Var(VarId::fresh());
                log::debug!("fresh let var: {:?}", var);

                self.ctx.push();
                let solved_pat = self.generate_pattern_constraints(src, pattern, &var, false)?;
                let solved_expr = self.generate_expr_constraints(src, expr)?;
                let solved_body = self.generate_expr_constraints(src, body)?;
                self.ctx.pop();

                Ok(Expr::new(
                    ExprKind::Let(solved_pat, true, solved_expr, solved_body.clone()),
                    (solved_body.meta.0, expr.meta),
                ))
            }
            nir::ExprKind::Let(pattern, false, expr, body) => {
                let solved_expr = self.generate_expr_constraints(src, expr)?;
                let solved_pat =
                    self.generate_pattern_constraints(src, pattern, &solved_expr.meta.0, false)?;
                let solved_body = self.generate_expr_constraints(src, body)?;

                Ok(Expr::new(
                    ExprKind::Let(solved_pat, false, solved_expr, solved_body.clone()),
                    (solved_body.meta.0, expr.meta),
                ))
            }
            nir::ExprKind::If(cond, then, else_) => {
                let solved_cond = self.generate_expr_constraints(src, cond)?;
                let solved_then = self.generate_expr_constraints(src, then)?;
                let solved_else = self.generate_expr_constraints(src, else_)?;

                self.constraints
                    .push(Constraint::Eq(solved_cond.meta.0.clone(), Ty::Bool));
                self.constraints.push(Constraint::Eq(
                    solved_then.meta.0.clone(),
                    solved_else.meta.0.clone(),
                ));

                Ok(Expr::new(
                    ExprKind::If(solved_cond, solved_then.clone(), solved_else),
                    (solved_then.meta.0, expr.meta),
                ))
            }
            nir::ExprKind::Match(e, arms) => {
                let solved_expr = self.generate_expr_constraints(src, &e)?;
                let ty = Ty::Var(VarId::fresh());
                log::debug!("fresh match var: {:?}", ty);

                let mut solved_arms = vec![];
                for (pat, body) in arms {
                    self.ctx.push();

                    let solved_pat =
                        self.generate_pattern_constraints(src, pat, &solved_expr.meta.0, false)?;
                    let solved_body = self.generate_expr_constraints(src, body)?;

                    self.constraints
                        .push(Constraint::Eq(solved_body.meta.0.clone(), ty.clone()));

                    solved_arms.push((solved_pat, solved_body));
                    self.ctx.pop();
                }

                Ok(Expr::new(
                    ExprKind::Match(solved_expr.clone(), solved_arms),
                    (ty, expr.meta),
                ))
            }
            nir::ExprKind::List(vec) => {
                let elem_ty = Ty::Var(VarId::fresh());
                log::debug!("fresh list var: {:?}", elem_ty);
                let list_ty = Ty::List(Box::new(elem_ty.clone()));
                let solved_vec = vec
                    .iter()
                    .map(|expr| self.generate_expr_constraints(src, expr))
                    .collect::<InferResult<Vec<Expr>>>()?;
                for expr in &solved_vec {
                    self.constraints
                        .push(Constraint::Eq(expr.meta.0.clone(), elem_ty.clone()));
                }

                Ok(Expr::new(ExprKind::List(solved_vec), (list_ty, expr.meta)))
            }
            nir::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, (Ty::Unit, expr.meta))),
        }
    }

    fn generate_pattern_constraints<'src>(
        &mut self,
        src: &'src str,
        pat: &nir::Pattern,
        ty: &Ty,
        generalize: bool,
    ) -> InferResult<Pattern> {
        match pat.value.as_ref() {
            nir::PatternKind::Wildcard => {
                Ok(Pattern::new(PatternKind::Wildcard, (ty.clone(), pat.meta)))
            }
            nir::PatternKind::Lit(lit) => Ok(match lit {
                nir::Lit::Byte(b) => {
                    Pattern::new(PatternKind::Lit(Lit::Byte(*b)), (Ty::Byte, pat.meta))
                }
                nir::Lit::Int(i) => {
                    Pattern::new(PatternKind::Lit(Lit::Int(*i)), (Ty::Int, pat.meta))
                }
                nir::Lit::Rational(r) => Pattern::new(
                    PatternKind::Lit(Lit::Rational(*r)),
                    (Ty::Rational, pat.meta),
                ),
                nir::Lit::Real(r) => {
                    Pattern::new(PatternKind::Lit(Lit::Real(*r)), (Ty::Real, pat.meta))
                }
                nir::Lit::Bool(b) => {
                    Pattern::new(PatternKind::Lit(Lit::Bool(*b)), (Ty::Bool, pat.meta))
                }
                nir::Lit::String(s) => Pattern::new(
                    PatternKind::Lit(Lit::String(s.clone())),
                    (Ty::String, pat.meta),
                ),
                nir::Lit::Char(c) => {
                    Pattern::new(PatternKind::Lit(Lit::Char(*c)), (Ty::Char, pat.meta))
                }
            }),
            nir::PatternKind::Ident(ident, type_hint) => {
                if generalize {
                    let scm = ty.generalize(self.ctx.free_vars());
                    self.ctx.insert(ident.value.name, scm.clone());
                    Ok(Pattern::new(
                        PatternKind::Ident(*ident),
                        (ty.clone(), pat.meta),
                    ))
                } else {
                    self.ctx
                        .insert(ident.value.name, Scheme::new(vec![], ty.clone()));
                    Ok(Pattern::new(
                        PatternKind::Ident(*ident),
                        (ty.clone(), pat.meta),
                    ))
                }
            }
            nir::PatternKind::List(vec) => {
                let v = VarId::fresh();
                let elem_ty = Ty::Var(v);
                log::debug!("fresh list pat var: {:?} is {:?}", elem_ty, v.get());
                let list_ty = Ty::List(Box::new(elem_ty.clone()));
                let solved_pats = vec
                    .iter()
                    .map(|pat| self.generate_pattern_constraints(src, pat, &elem_ty, generalize))
                    .collect::<InferResult<Vec<Pattern>>>()?;
                self.constraints
                    .push(Constraint::Eq(ty.clone(), list_ty.clone()));
                Ok(Pattern::new(
                    PatternKind::List(solved_pats),
                    (list_ty, pat.meta),
                ))
            }
            nir::PatternKind::Pair(head, tail) => {
                let var = Ty::Var(VarId::fresh());
                log::debug!("fresh pair pat var: {:?}", var);
                let pair_ty = Ty::List(Box::new(var.clone()));
                let solved_head = self.generate_pattern_constraints(src, head, &var, generalize)?;
                let solved_tail =
                    self.generate_pattern_constraints(src, tail, &pair_ty, generalize)?;
                self.constraints
                    .push(Constraint::Eq(ty.clone(), pair_ty.clone()));
                Ok(Pattern::new(
                    PatternKind::Pair(solved_head, solved_tail),
                    (pair_ty, pat.meta),
                ))
            }
            nir::PatternKind::Unit => Ok(Pattern::new(PatternKind::Unit, (Ty::Unit, pat.meta))),
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
        match &decl.value {
            DeclKind::Def(def) => decl.map_ref(|_| DeclKind::Def(self.zonk_def(def))),
            DeclKind::DefGroup(defs) => decl.map_ref(|_| {
                DeclKind::DefGroup(defs.iter().map(|def| self.zonk_def(def)).collect_vec())
            }),
        }
    }

    fn zonk_def(&mut self, def: &Def) -> Def {
        match &def.value {
            DefKind::Rec { ident, body } => Def::new(
                DefKind::Rec {
                    ident: ident.clone(),
                    body: self.zonk_expr(body),
                },
                (def.meta.0.zonk(), def.meta.1),
            ),
            DefKind::NonRec { pat, body } => Def::new(
                DefKind::NonRec {
                    pat: self.zonk_pattern(pat),
                    body: self.zonk_expr(body),
                },
                (def.meta.0.zonk(), def.meta.1),
            ),
        }
    }

    fn zonk_expr(&mut self, expr: &Expr) -> Expr {
        match expr.value.as_ref() {
            ExprKind::Lit(lit) => Expr::new(
                ExprKind::Lit(lit.clone()),
                (expr.meta.0.zonk(), expr.meta.1),
            ),
            ExprKind::Var(node) => Expr::new(
                ExprKind::Var(node.clone()),
                (expr.meta.0.zonk(), expr.meta.1),
            ),
            ExprKind::Apply(fun, arg) => Expr::new(
                ExprKind::Apply(self.zonk_expr(fun), self.zonk_expr(arg)),
                (expr.meta.0.zonk(), expr.meta.1),
            ),
            ExprKind::Lambda(param, expr) => Expr::new(
                ExprKind::Lambda(self.zonk_pattern(param), self.zonk_expr(expr)),
                (expr.meta.0.zonk(), expr.meta.1),
            ),
            ExprKind::Or(lhs, rhs) => Expr::new(
                ExprKind::Or(self.zonk_expr(lhs), self.zonk_expr(rhs)),
                (expr.meta.0.zonk(), expr.meta.1),
            ),
            ExprKind::And(lhs, rhs) => Expr::new(
                ExprKind::And(self.zonk_expr(lhs), self.zonk_expr(rhs)),
                (expr.meta.0.zonk(), expr.meta.1),
            ),
            ExprKind::Let(pat, _, expr, body) => Expr::new(
                ExprKind::Let(
                    self.zonk_pattern(pat),
                    true,
                    self.zonk_expr(expr),
                    self.zonk_expr(body),
                ),
                (expr.meta.0.zonk(), expr.meta.1),
            ),
            ExprKind::If(cond, then, else_) => Expr::new(
                ExprKind::If(
                    self.zonk_expr(cond),
                    self.zonk_expr(then),
                    self.zonk_expr(else_),
                ),
                (expr.meta.0.zonk(), expr.meta.1),
            ),
            ExprKind::Match(expr, cases) => Expr::new(
                ExprKind::Match(
                    self.zonk_expr(expr),
                    cases
                        .iter()
                        .map(|(pat, body)| (self.zonk_pattern(pat), self.zonk_expr(body)))
                        .collect_vec(),
                ),
                (expr.meta.0.zonk(), expr.meta.1),
            ),
            ExprKind::List(xs) => Expr::new(
                ExprKind::List(xs.iter().map(|x| self.zonk_expr(x)).collect_vec()),
                (expr.meta.0.zonk(), expr.meta.1),
            ),
            ExprKind::Unit => Expr::new(ExprKind::Unit, (Ty::Unit, expr.meta.1)),
        }
    }

    fn zonk_pattern(&mut self, pat: &Pattern) -> Pattern {
        match pat.value.as_ref() {
            PatternKind::Wildcard => {
                Pattern::new(PatternKind::Wildcard, (pat.meta.0.zonk(), pat.meta.1))
            }
            PatternKind::Lit(lit) => Pattern::new(
                PatternKind::Lit(lit.clone()),
                (pat.meta.0.zonk(), pat.meta.1),
            ),
            PatternKind::Ident(ident) => Pattern::new(
                PatternKind::Ident(ident.clone()),
                (pat.meta.0.zonk(), pat.meta.1),
            ),
            PatternKind::List(xs) => Pattern::new(
                PatternKind::List(xs.iter().map(|x| self.zonk_pattern(x)).collect_vec()),
                (pat.meta.0.zonk(), pat.meta.1),
            ),
            PatternKind::Pair(head, tail) => Pattern::new(
                PatternKind::Pair(self.zonk_pattern(head), self.zonk_pattern(tail)),
                (pat.meta.0.zonk(), pat.meta.1),
            ),
            PatternKind::Unit => Pattern::new(PatternKind::Unit, (pat.meta.0.zonk(), pat.meta.1)),
        }
    }
}
