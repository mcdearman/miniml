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
    
    pub fn add_edges(&mut self, from: T, to: Vec<T>) {
        self.0.entry(from).or_insert_with(Vec::new).extend(to);
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
    pub fn scc(&mut self) -> Vec<Vec<T>> {
        let mut index = 0;
        let mut stack = Vec::new();
        let mut indices = BTreeMap::new();
        let mut lowlinks = BTreeMap::new();
        let mut on_stack = BTreeMap::new();
        let mut sccs = Vec::new();

        fn strongconnect<T: Clone + Ord>(
            node: T,
            graph: &Graph<T>,
            index: &mut usize,
            stack: &mut Vec<T>,
            indices: &mut BTreeMap<T, usize>,
            lowlinks: &mut BTreeMap<T, usize>,
            on_stack: &mut BTreeMap<T, bool>,
            sccs: &mut Vec<Vec<T>>,
        ) {
            indices.insert(node.clone(), *index);
            lowlinks.insert(node.clone(), *index);
            *index += 1;
            stack.push(node.clone());
            on_stack.insert(node.clone(), true);

            if let Some(neighbors) = graph.0.get(&node) {
                for neighbor in neighbors {
                    if !indices.contains_key(neighbor) {
                        strongconnect(
                            neighbor.clone(),
                            graph,
                            index,
                            stack,
                            indices,
                            lowlinks,
                            on_stack,
                            sccs,
                        );
                        lowlinks.insert(
                            node.clone(),
                            std::cmp::min(lowlinks[&node], lowlinks[neighbor]),
                        );
                    } else if on_stack[neighbor] {
                        lowlinks.insert(
                            node.clone(),
                            std::cmp::min(lowlinks[&node], indices[neighbor]),
                        );
                    }
                }
            }

            if lowlinks[&node] == indices[&node] {
                let mut scc = Vec::new();
                loop {
                    let n = stack.pop().unwrap();
                    on_stack.insert(n.clone(), false);
                    scc.push(n.clone());
                    if n == node {
                        break;
                    }
                }
                sccs.push(scc);
            }
        }

        for node in self.0.keys() {
            if !indices.contains_key(node) {
                strongconnect(
                    node.clone(),
                    self,
                    &mut index,
                    &mut stack,
                    &mut indices,
                    &mut lowlinks,
                    &mut on_stack,
                    &mut sccs,
                );
            }
        }

        sccs
    }
}
