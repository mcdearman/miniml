use std::{
    fmt::Display,
    ops::{Index, Range},
};

#[derive(Debug, Clone, Copy, Eq, PartialEq, Default, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<{}, {}>", self.start, self.end)
    }
}

impl From<Span> for Range<usize> {
    fn from(span: Span) -> Self {
        span.start as usize..span.end as usize
    }
}

impl From<Range<usize>> for Span {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start as u32,
            end: range.end as u32,
        }
    }
}

impl Index<Span> for str {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[Range::from(index)]
    }
}

impl Index<Span> for String {
    type Output = str;

    fn index(&self, index: Span) -> &Self::Output {
        &self[Range::from(index)]
    }
}

pub type Spanned<T> = (T, Span);
