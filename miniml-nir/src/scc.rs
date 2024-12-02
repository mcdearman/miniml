use crate::*;
use miniml_utils::scc::Graph;

impl Module {
    pub fn scc(&self) -> Self {
        let mut graph = Graph::new();

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
