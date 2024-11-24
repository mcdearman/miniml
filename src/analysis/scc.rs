/// Implement Tarjan's algorithm for finding strongly connected components.

pub fn tarjan<'a, T: Hash + Eq + Clone>(graph: &'a HashMap<T, Vec<T>>) -> Vec<Vec<&'a T>> {
    let mut index = 0;
    let mut stack = Vec::new();
    let mut indices = HashMap::new();
    let mut lowlinks = HashMap::new();
    let mut on_stack = HashSet::new();
    let mut sccs = Vec::new();

    for node in graph.keys() {
        if !indices.contains_key(node) {
            tarjan_visit(
                graph,
                node,
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
