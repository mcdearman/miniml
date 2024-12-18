#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Node<T, M = ()> {
    pub value: T,
    pub meta: M,
}

impl<T, M: Clone> Node<T, M> {
    pub fn new(value: T, meta: M) -> Self {
        Self { value, meta }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Node<U, M> {
        Node {
            value: f(self.value),
            meta: self.meta,
        }
    }

    pub fn map_ref<U, F: FnOnce(&T) -> U>(&self, f: F) -> Node<U, M> {
        Node {
            value: f(&self.value),
            meta: self.meta.clone(),
        }
    }
}
