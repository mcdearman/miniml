use super::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Node<T, M = ()> {
    pub kind: T,
    pub meta: M,
}

impl<T, M: Clone> Node<T, M> {
    pub fn new(kind: T, meta: M) -> Self {
        Self { kind, meta }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Node<U, M> {
        Node {
            kind: f(self.kind),
            meta: self.meta,
        }
    }

    pub fn map_ref<U, F: FnOnce(&T) -> U>(&self, f: F) -> Node<U, M> {
        Node {
            kind: f(&self.kind),
            meta: self.meta.clone(),
        }
    }
}
