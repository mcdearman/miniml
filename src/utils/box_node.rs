use super::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct BoxNode<T> {
    pub kind: Box<T>,
    pub span: Span,
}

impl<T> BoxNode<T> {
    pub fn new(kind: T, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            span,
        }
    }

    pub fn kind(&self) -> &T {
        &*self.kind
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> BoxNode<U> {
        BoxNode {
            kind: Box::new(f(*self.kind)),
            span: self.span,
        }
    }

    pub fn map_ref<U, F: FnOnce(&T) -> U>(&self, f: F) -> BoxNode<U> {
        BoxNode {
            kind: Box::new(f(&*self.kind)),
            span: self.span,
        }
    }
}
