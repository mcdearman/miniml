#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BoxNode<T, M = ()> {
    pub value: Box<T>,
    pub meta: M,
}

impl<T, M: Clone> BoxNode<T, M> {
    pub fn new(value: T, meta: M) -> Self {
        Self {
            value: Box::new(value),
            meta,
        }
    }

    pub fn value(&self) -> &T {
        &self.value
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> BoxNode<U, M> {
        BoxNode {
            value: Box::new(f(*self.value)),
            meta: self.meta,
        }
    }

    pub fn map_ref<U, F: FnOnce(&T) -> U>(&self, f: F) -> BoxNode<U, M> {
        BoxNode {
            value: Box::new(f(&*self.value)),
            meta: self.meta.clone(),
        }
    }

    pub fn map_meta<N, F: FnOnce(M) -> N>(self, f: F) -> BoxNode<T, N> {
        BoxNode {
            value: self.value,
            meta: f(self.meta.clone()),
        }
    }
}
