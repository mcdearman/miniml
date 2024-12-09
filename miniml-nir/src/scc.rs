use crate::*;
use itertools::Itertools;
use miniml_utils::graph::Graph;
use std::collections::HashMap;

impl Module {
    pub fn scc(&self) -> Self {
        let mut graph = Graph::new();
        let mut decl_map: HashMap<ScopedIdent, Def> = HashMap::new();

        let top_level_names = self
            .decls
            .iter()
            .filter_map(|decl| match &decl.value {
                DeclKind::Def(def) => match &def.value {
                    DefKind::Rec { ident, .. } => {
                        decl_map.insert(ident.value.clone(), def.clone());
                        Some(ident.value)
                    }
                    DefKind::NonRec { pat, .. } => match pat.value.as_ref() {
                        PatternKind::Ident(ident, _) => {
                            decl_map.insert(ident.value.clone(), def.clone());
                            Some(ident.value)
                        }
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            })
            .collect::<Vec<_>>();

        for decl in &self.decls {
            match &decl.value {
                DeclKind::Def(def) => match &def.value {
                    DefKind::Rec { ident, body, .. } => {
                        let scc = body.value.scc(top_level_names.clone());
                        graph.add_edges(ident.value.clone(), scc);
                    }
                    _ => {}
                },
                _ => {}
            }
        }

        Self {
            name: self.name.clone(),
            imports: self.imports.clone(),
            decls: graph
                .scc()
                .iter()
                .map(|scc| {
                    let decls = scc
                        .iter()
                        .map(|ident| decl_map.get(ident).unwrap().clone())
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
        }
    }
}

impl ExprKind {
    pub fn scc(&self, top_level_names: Vec<ScopedIdent>) -> Vec<ScopedIdent> {
        match self {
            ExprKind::Apply(fun, arg) => {
                if let ExprKind::Var(ident) = fun.value.as_ref() {
                    if top_level_names.contains(&ident.value) {
                        vec![ident.value.clone()]
                            .into_iter()
                            .chain(arg.value.scc(top_level_names))
                            .collect_vec()
                    } else {
                        vec![]
                    }
                } else {
                    let fun_names = fun.value.scc(top_level_names.clone());
                    let arg_names = arg.value.scc(top_level_names.clone());
                    fun_names
                        .into_iter()
                        .chain(arg_names.into_iter())
                        .collect_vec()
                }
            }
            ExprKind::Lambda(_, expr) => expr.value.scc(top_level_names.clone()),
            ExprKind::Or(lhs, rhs) => lhs
                .value
                .scc(top_level_names.clone())
                .into_iter()
                .chain(rhs.value.scc(top_level_names.clone()).into_iter())
                .collect_vec(),
            ExprKind::And(lhs, rhs) => lhs
                .value
                .scc(top_level_names.clone())
                .into_iter()
                .chain(rhs.value.scc(top_level_names.clone()).into_iter())
                .collect_vec(),
            ExprKind::Let(_, _, expr, body) => expr
                .value
                .scc(top_level_names.clone())
                .into_iter()
                .chain(body.value.scc(top_level_names.clone()).into_iter())
                .collect_vec(),
            ExprKind::If(cond, then, else_) => cond
                .value
                .scc(top_level_names.clone())
                .into_iter()
                .chain(then.value.scc(top_level_names.clone()).into_iter())
                .chain(else_.value.scc(top_level_names.clone()).into_iter())
                .collect_vec(),
            ExprKind::Match(expr, arms) => {
                let expr_names = expr.value.scc(top_level_names.clone());
                let arm_names = arms
                    .iter()
                    .map(|arm| arm.1.value.scc(top_level_names.clone()))
                    .flatten()
                    .collect_vec();
                expr_names
                    .into_iter()
                    .chain(arm_names.into_iter())
                    .collect_vec()
            }
            ExprKind::List(elems) => elems
                .iter()
                .map(|elem| elem.value.scc(top_level_names.clone()))
                .flatten()
                .collect_vec(),
            _ => vec![],
        }
    }
}
