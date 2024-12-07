use crate::*;
use miniml_utils::graph::Graph;

impl Module {
    pub fn scc(&self) -> Self {
        let mut graph = Graph::new();

        let top_level_names = self
            .decls
            .iter()
            .filter_map(|decl| match &decl.value {
                DeclKind::Def(def_group) => match def_group {
                    DefGroup::NonRec(def) => match def {
                        Def::Rec { ident, .. } => Some(ident.value),
                        Def::NonRec { pat, .. } => match pat.value.as_ref() {
                            PatternKind::Ident(ident, _) => Some(ident.value),
                            _ => None,
                        },
                    },
                    _ => None,
                },
            })
            .collect::<Vec<_>>();

        for decl in &self.decls {
            match &decl.value {
                DeclKind::Def(def_group) => match def_group {
                    DefGroup::NonRec(def) => match &def {
                        Def::Rec { ident, body, .. } => {
                            // graph.add_edge(ident.clone(), body.clone());
                        }
                        Def::NonRec { pat, body } => {
                            // graph.add_edge(pat.clone(), body.clone());
                        }
                    },
                    _ => {}
                },
            }
        }

        todo!()
    }
}

impl ExprKind {
    pub fn scc(&self) -> Self {
        todo!()
    }
}
