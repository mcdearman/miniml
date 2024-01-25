#[derive(Debug, Clone, PartialEq)]
struct BTree<K, V> {
    root: Box<Node<K, V>>,
    order: usize,
}

#[derive(Debug, Clone, PartialEq)]
struct Node<K, V> {
    keys: Vec<K>,
    children: Vec<Box<Node<K, V>>>,
    values: Vec<V>,
    leaf: bool,
}
