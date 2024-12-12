use crate::*;
use itertools::Itertools;
use miniml_utils::graph::Graph;

#[derive(Debug)]
pub struct SCC {
    graph: Graph<usize>,
    decls: Vec<Def>,
    top_level_names: Vec<ScopedIdent>,
}

impl SCC {
    pub fn new() -> Self {
        Self {
            graph: Graph::new(),
            decls: vec![],
            top_level_names: vec![],
        }
    }

    pub fn scc(&mut self, prog: &Prog) -> Prog {
        self.top_level_names = self
            .decls
            .clone()
            .into_iter()
            .filter_map(|def| self.search_defs(&def))
            .flatten()
            .collect::<Vec<_>>();

        for decl in &prog.value.decls {
            match &decl.value {
                DeclKind::Def(def) => match &def.value {
                    DefKind::Rec { ident, body, .. } => {
                        let scc = self.scc_expr(body);
                        self.graph.add_edges(self.id, scc);
                    }
                    _ => {}
                },
                _ => {}
            }
        }

        SynNode::new(
            Module {
                name: prog.value.name.clone(),
                imports: prog.value.imports.clone(),
                decls: self
                    .graph
                    .scc()
                    .iter()
                    .map(|scc| {
                        let decls = scc
                            .iter()
                            .map(|ident| self.decls.get(ident).unwrap().clone())
                            .enumerate()
                            .sorted_by(|(i, _), (j, _)| j.cmp(i))
                            .map(|(_, (_, def))| def)
                            .collect_vec();

                        if decls.len() == 1 {
                            Decl::new(DeclKind::Def(decls[0].clone()), decls[0].meta)
                        } else {
                            Decl::new(
                                DeclKind::DefGroup(decls.clone()),
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
        match expr.value.as_ref() {
            ExprKind::Apply(fun, arg) => {
                if let ExprKind::Var(ident) = fun.value.as_ref() {
                    if self.top_level_names.contains(&ident.value) {
                        let id = self.decls.
                        vec![ident.value.clone()]
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

    fn search_defs(&mut self, def: &Def) -> Option<Vec<ScopedIdent>> {
        match &def.value {
            DefKind::Rec { ident, .. } => {
                self.decls.insert(self.id, (self.id, def.clone()));
                self.id += 1;
                Some(vec![ident.value])
            }
            DefKind::NonRec { pat, .. } => match pat.value.as_ref() {
                PatternKind::Ident(ident, _) => {
                    self.decls.insert(self.id, (self.id, def.clone()));
                    self.id += 1;
                    Some(vec![ident.value])
                }
                _ => None,
            },
        }
    }
}
