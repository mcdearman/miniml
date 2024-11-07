use super::{functor::Functor, span::Span};

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

impl<T: Clone, M: Clone> Functor for Node<T, M> {
    type Wrapped<U: Clone> = T;

    fn map<F, U: Clone, N: Clone>(f: F, x: U) -> N
    where
        F: FnOnce(T) -> U,
    {
        Node {
            kind: f(x),
            meta: 
        }
    }
}
