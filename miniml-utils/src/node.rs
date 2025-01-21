#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Node<T, M = ()> {
    pub inner: T,
    pub meta: M,
}

impl<T, M: Clone> Node<T, M> {
    pub fn new(value: T, meta: M) -> Self {
        Self { inner: value, meta }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Node<U, M> {
        Node {
            inner: f(self.inner),
            meta: self.meta,
        }
    }

    pub fn map_ref<U, F: FnOnce(&T) -> U>(&self, f: F) -> Node<U, M> {
        Node {
            inner: f(&self.inner),
            meta: self.meta.clone(),
        }
    }

    pub fn map_meta<N, F: FnOnce(M) -> N>(self, f: F) -> Node<T, N> {
        Node {
            inner: self.inner,
            meta: f(self.meta.clone()),
        }
    }
}
