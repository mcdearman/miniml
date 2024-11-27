use std::collections::BTreeMap;

#[derive(Debug, Clone, PartialEq)]
pub struct Graph<T>(BTreeMap<T, Vec<T>>);

impl<T: Clone + Ord> Graph<T> {
    pub fn new() -> Self {
        Graph(BTreeMap::new())
    }

    pub fn add_node(&mut self, node: T) {
        self.0.entry(node).or_insert_with(Vec::new);
    }

    pub fn add_edge(&mut self, from: T, to: T) {
        self.0.entry(from).or_insert_with(Vec::new).push(to);
    }

    pub fn remove_edge(&mut self, from: T, to: T) {
        if let Some(edges) = self.0.get_mut(&from) {
            edges.retain(|x| *x != to);
        }
    }

    pub fn remove_node(&mut self, node: T) {
        self.0.remove(&node);
        for edges in self.0.values_mut() {
            edges.retain(|x| *x != node);
        }
    }

    // Implement Tarjan's algorithm for finding strongly connected components.
    pub fn scc(&mut self) {}
}
