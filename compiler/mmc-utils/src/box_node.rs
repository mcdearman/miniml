use std::ops::Deref;

use crate::pretty::Pretty;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct BoxNode<T, M = ()> {
    pub inner: Box<T>,
    pub meta: M,
}

impl<T, M: Clone> BoxNode<T, M> {
    pub fn new(value: T, meta: M) -> Self {
        Self {
            inner: Box::new(value),
            meta,
        }
    }

    pub fn inner(&self) -> &T {
        &self.inner
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> BoxNode<U, M> {
        BoxNode {
            inner: Box::new(f(*self.inner)),
            meta: self.meta,
        }
    }

    pub fn map_ref<U, F: FnOnce(&T) -> U>(&self, f: F) -> BoxNode<U, M> {
        BoxNode {
            inner: Box::new(f(&*self.inner)),
            meta: self.meta.clone(),
        }
    }

    pub fn map_meta<N, F: FnOnce(M) -> N>(self, f: F) -> BoxNode<T, N> {
        BoxNode {
            inner: self.inner,
            meta: f(self.meta.clone()),
        }
    }
}

impl<T, M> Deref for BoxNode<T, M> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<T: Pretty, M: Pretty> Pretty for BoxNode<T, M> {
    fn pretty(&self) -> String {
        format!("{} {}", self.inner.pretty(), self.meta.pretty())
    }
}
