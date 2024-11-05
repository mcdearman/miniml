use super::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Node<T> {
    pub kind: T,
    pub span: Span,
}

impl<T> Node<T> {
    pub fn new(kind: T, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Node<U> {
        Node {
            kind: f(self.kind),
            span: self.span,
        }
    }

    pub fn map_ref<U, F: FnOnce(&T) -> U>(&self, f: F) -> Node<U> {
        Node {
            kind: f(&self.kind),
            span: self.span,
        }
    }
}
