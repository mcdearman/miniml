use super::span::Span;
use std::{
    cmp::Ordering,
    fmt,
    ops::{Deref, DerefMut},
};

#[derive(Clone)]
pub struct Node<T> {
    inner: Box<T>, // TODO: Replace with smallbox or similar optimisation?
    span: Span,
}

impl<T> Node<T> {
    /// Create a new node with the given inner value and metadata.
    pub fn new(inner: T, span: Span) -> Self {
        Node {
            inner: Box::new(inner),
            span,
        }
    }

    /// Get a reference to the inner value.
    pub fn inner(&self) -> &T {
        &self.inner
    }

    /// Get a mutable to the inner value.
    pub fn inner_mut(&mut self) -> &mut T {
        &mut self.inner
    }

    // Take the node's inner value.
    pub fn into_inner(self) -> T {
        *self.inner
    }

    pub fn span(&self) -> Span {
        self.span
    }

    /// Map the node's inner value.
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Node<U> {
        Node {
            inner: Box::new(f(*self.inner)),
            span: self.span,
        }
    }

    pub fn try_map<U, E, F: FnOnce(T) -> Result<U, E>>(self, f: F) -> Result<Node<U>, E> {
        Ok(Node {
            inner: Box::new(f(*self.inner)?),
            span: self.span,
        })
    }
}

impl<T> Deref for Node<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        self.inner()
    }
}

impl<T> DerefMut for Node<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        self.inner_mut()
    }
}

impl<T: PartialEq> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        // Only compare inner
        self.inner == other.inner
    }
}

impl<T: Eq> Eq for Node<T> {}

impl<T: Ord> Ord for Node<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.inner.cmp(&other.inner)
    }
}

impl<T: PartialOrd> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.inner.partial_cmp(&other.inner)
    }
}

impl<T: fmt::Debug> fmt::Debug for Node<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:#?} @ {:?}", self.inner, self.span)
    }
}
