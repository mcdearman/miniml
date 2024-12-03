use crate::*;
use miniml_utils::scc::Graph;

impl Module {
    pub fn scc(&self) -> Self {
        let mut graph = Graph::new();

        let top_level_names = self
            .decls
            .iter()
            .filter_map(|decl| match &decl.value {
                DeclKind::Def(def_group) => match def_group {
                    DefGroup::NonRec(def) => match def {
                        Def::Rec { ident, .. } => Some(ident.clone()),
                        Def::NonRec { pat, .. } => match pat.value.as_ref() {
                            PatternKind::Ident(ident, _) => Some(ident.clone()),
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
                    DefGroup::Rec(defs) => {
                        for def in defs {
                            // graph.add_node(def..clone());
                        }
                        for def in defs {
                            match &def {
                                Def::Rec { ident, body, .. } => {
                                    graph.add_edge(ident.clone(), body.clone());
                                }
                                Def::NonRec { pat, body } => {
                                    graph.add_edge(pat.clone(), body.clone());
                                }
                            }
                        }
                    }
                    DefGroup::NonRec(def) => match &def {
                        Def::Rec { ident, body, .. } => {
                            graph.add_edge(ident.clone(), body.clone());
                        }
                        Def::NonRec { pat, body } => {
                            graph.add_edge(pat.clone(), body.clone());
                        }
                    },
                },
            }
        }
        todo!()
    }
}
