use std::ops::{Index, Range};

#[derive(Debug, Clone, PartialEq)]
pub struct Loc {
    pub start: u32,
    pub end: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Located<T> {
    pub value: T,
    pub loc: Loc,
}

impl From<Loc> for Range<usize> {
    fn from(loc: Loc) -> Self {
        loc.start as usize..loc.end as usize
    }
}

impl From<Range<usize>> for Loc {
    fn from(range: Range<usize>) -> Self {
        Self {
            start: range.start as u32,
            end: range.end as u32,
        }
    }
}

impl Index<Loc> for str {
    type Output = str;

    fn index(&self, index: Loc) -> &Self::Output {
        // Convert the Span to a Range<usize>
        let range: Range<usize> = index.into();

        // Ensure the range is within the bounds of the string
        if range.start > self.len() || range.end > self.len() {
            panic!("Index out of bounds");
        }

        // Return the slice of the string for the given range
        &self[range]
    }
}
