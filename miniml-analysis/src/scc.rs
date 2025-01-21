use itertools::Itertools;
use miniml_ast::SynNode;
use miniml_nir::scoped_ident::ScopedIdent;
use miniml_nir::*;
use miniml_utils::graph::Graph;
use std::collections::HashMap;

#[derive(Debug)]
pub struct SCC {
    graph: Graph<usize>,
    decl_map: HashMap<ScopedIdent, usize>,
    top_level_names: Vec<ScopedIdent>,
}

impl SCC {
    pub fn new() -> Self {
        Self {
            graph: Graph::new(),
            decl_map: HashMap::new(),
            top_level_names: vec![],
        }
    }

    pub fn run(&mut self, prog: &Prog) -> Prog {
        self.top_level_names = prog
            .inner
            .decls
            .clone()
            .into_iter()
            .enumerate()
            .filter_map(|(i, decl)| self.search_decls(&decl, i))
            .flatten()
            .collect::<Vec<_>>();

        for (i, decl) in prog.inner.decls.iter().enumerate() {
            match &decl.inner {
                DeclKind::Def(def) => match &def.inner {
                    DefKind::Rec { ident: _, body, .. } => {
                        let scc = self.scc_expr(body);
                        self.graph.add_edges(i, scc);
                    }
                    DefKind::NonRec { pat, body: _ } => {
                        self.graph.add_edges(i, vec![]);
                        if let Some(names) = self.search_decl_pats(pat) {
                            self.top_level_names.extend(names);
                        }
                    }
                },
                _ => {}
            }
        }

        SynNode::new(
            Module {
                name: prog.inner.name.clone(),
                imports: prog.inner.imports.clone(),
                decls: self
                    .graph
                    .scc()
                    .iter()
                    .map(|scc| {
                        let decls = scc
                            .iter()
                            .map(|id| {
                                prog.inner
                                    .decls
                                    .get(*id)
                                    .expect(&*format!("id {} out of bounds", id))
                                    .clone()
                            })
                            .enumerate()
                            .sorted_by(|(i, _), (j, _)| j.cmp(i))
                            .map(|(_, decl)| decl)
                            .collect_vec();

                        if decls.len() == 1 {
                            decls[0].clone()
                        } else {
                            Decl::new(
                                DeclKind::DefGroup(
                                    decls
                                        .iter()
                                        .map(|decl| match &decl.inner {
                                            DeclKind::Def(def) => vec![def.clone()],
                                            DeclKind::DefGroup(defs) => defs.clone(),
                                        })
                                        .flatten()
                                        .collect_vec(),
                                ),
                                decls[0].meta.extend(decls.last().unwrap().meta),
                            )
                        }
                    })
                    .collect_vec(),
            },
            prog.meta,
        )
    }

    fn scc_expr(&self, expr: &Expr) -> Vec<usize> {
        match expr.inner.as_ref() {
            ExprKind::Apply(fun, arg) => {
                if let ExprKind::Var(ident) = fun.inner.as_ref() {
                    if self.top_level_names.contains(&ident.inner) {
                        let tid = self.decl_map.get(&ident.inner).expect("decl not found");
                        vec![*tid]
                            .into_iter()
                            .chain(self.scc_expr(arg))
                            .collect_vec()
                    } else {
                        vec![]
                    }
                } else {
                    let fun_ids = self.scc_expr(fun);
                    let arg_ids = self.scc_expr(arg);
                    fun_ids.into_iter().chain(arg_ids.into_iter()).collect_vec()
                }
            }
            ExprKind::Lambda(_, expr) => self.scc_expr(expr),
            ExprKind::Or(lhs, rhs) => self
                .scc_expr(lhs)
                .into_iter()
                .chain(self.scc_expr(rhs).into_iter())
                .collect_vec(),
            ExprKind::And(lhs, rhs) => self
                .scc_expr(lhs)
                .into_iter()
                .chain(self.scc_expr(rhs).into_iter())
                .collect_vec(),
            ExprKind::Let(_, _, expr, body) => self
                .scc_expr(expr)
                .into_iter()
                .chain(self.scc_expr(body).into_iter())
                .collect_vec(),
            ExprKind::If(cond, then, else_) => self
                .scc_expr(cond)
                .into_iter()
                .chain(self.scc_expr(then).into_iter())
                .chain(self.scc_expr(else_).into_iter())
                .collect_vec(),
            ExprKind::Match(expr, arms) => {
                let expr_names = self.scc_expr(expr);
                let arm_names = arms
                    .iter()
                    .map(|arm| self.scc_expr(&arm.1))
                    .flatten()
                    .collect_vec();
                expr_names
                    .into_iter()
                    .chain(arm_names.into_iter())
                    .collect_vec()
            }
            ExprKind::List(elems) => elems
                .iter()
                .map(|elem| self.scc_expr(elem))
                .flatten()
                .collect_vec(),
            _ => vec![],
        }
    }

    fn search_decls(&mut self, decl: &Decl, i: usize) -> Option<Vec<ScopedIdent>> {
        match &decl.inner {
            DeclKind::Def(def) => match &def.inner {
                DefKind::Rec { ident, .. } => {
                    self.decl_map.insert(ident.inner.clone(), i);
                    Some(vec![ident.inner])
                }
                DefKind::NonRec { pat, .. } => self.search_decl_pats(pat),
            },
            _ => None,
        }
    }

    fn search_decl_pats(&mut self, pat: &Pattern) -> Option<Vec<ScopedIdent>> {
        match pat.inner.as_ref() {
            PatternKind::Ident(ident, _) => Some(vec![ident.inner]),
            PatternKind::List(xs) => Some(
                xs.iter()
                    .map(|x| self.search_decl_pats(x))
                    .flatten()
                    .flatten()
                    .collect_vec(),
            ),
            PatternKind::Pair(head, tail) => {
                let head = self.search_decl_pats(head);
                let tail = self.search_decl_pats(tail);
                match (head, tail) {
                    (Some(head), Some(tail)) => {
                        Some(head.into_iter().chain(tail.into_iter()).collect_vec())
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }
}
