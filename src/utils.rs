use lasso::{Spur, ThreadedRodeo};
use num_rational::Rational64;
use once_cell::sync::Lazy;
use std::error::Error;
use std::ops::{Index, Range};
use std::{borrow::Borrow, ops::Deref};
use std::{
    fmt::{Debug, Display},
    str::FromStr,
    sync::atomic::{AtomicUsize, Ordering},
};

#[derive(Clone, Copy, Eq, PartialEq, Default, Hash)]
pub struct Span {
    start: u32,
    end: u32,
}

impl Span {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub fn start(&self) -> u32 {
        self.start
    }

    pub fn end(&self) -> u32 {
        self.end
    }

    pub fn extend(&self, other: Span) -> Self {
        Self {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
        }
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
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

impl chumsky::span::Span for Span {
    type Context = ();

    type Offset = u32;

    fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
        Self {
            start: range.start,
            end: range.end,
        }
    }

    fn context(&self) -> Self::Context {
        ()
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
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

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct UniqueId(pub usize);

static COUNTER: AtomicUsize = AtomicUsize::new(0);

impl UniqueId {
    pub fn gen() -> Self {
        Self(COUNTER.fetch_add(1, Ordering::SeqCst))
    }
}

impl Debug for UniqueId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "UniqueId({})", self.0)
    }
}

impl From<usize> for UniqueId {
    fn from(id: usize) -> Self {
        Self(id)
    }
}

impl From<UniqueId> for usize {
    fn from(id: UniqueId) -> Self {
        id.0
    }
}

impl PartialEq<usize> for UniqueId {
    fn eq(&self, other: &usize) -> bool {
        self.0 == *other
    }
}

impl PartialOrd<usize> for UniqueId {
    fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(other)
    }
}

#[derive(Debug, Clone, PartialEq, Default)]
pub enum List<T> {
    #[default]
    Empty,
    Pair {
        head: T,
        tail: Box<Self>,
    },
}

impl<T> List<T> {
    pub fn push_front(&mut self, head: T) {
        let tail = std::mem::replace(self, Self::Empty);
        *self = Self::Pair {
            head,
            tail: Box::new(tail),
        };
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        ListIter::new(self)
    }
}

impl<T, I> From<I> for List<T>
where
    I: IntoIterator<Item = T>,
    <I as IntoIterator>::IntoIter: DoubleEndedIterator,
{
    fn from(value: I) -> Self {
        let mut list = List::Empty;
        for item in value.into_iter().rev() {
            list.push_front(item);
        }
        list
    }
}

#[derive(Debug)]
pub struct ListIter<'a, T> {
    list: &'a List<T>,
}

impl<'a, T> ListIter<'a, T> {
    pub fn new(list: &'a List<T>) -> Self {
        Self { list }
    }
}

impl<'a, T> Iterator for ListIter<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.list {
            List::Empty => None,
            List::Pair { head, tail } => {
                self.list = tail;
                Some(head)
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq)]
pub struct Num(pub Rational64);

impl Display for Num {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Debug for Num {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for Num {
    type Err = num_rational::ParseRatioError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        // let nums = s.split("/");
        // if nums.clone().count() == 2 {
        //     let numer = nums.clone().nth(0).unwrap();
        //     let denom = nums.clone().nth(1).unwrap();
        //     match
        // } else {
        //     Ok(Self(Rational64::from_str(s)?))
        // }
        Rational64::from_str(s).map(Self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ParseRatioError;

impl ParseRatioError {
    pub fn desc(&self) -> &str {
        "failed to parse ratio"
    }
}

impl Display for ParseRatioError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.desc())
    }
}

impl Error for ParseRatioError {
    fn description(&self) -> &str {
        "failed to parse ratio"
    }
}

pub static mut INTERNER: Lazy<ThreadedRodeo> = Lazy::new(|| ThreadedRodeo::default());

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct InternedString {
    key: Spur,
}

impl From<Spur> for InternedString {
    fn from(key: Spur) -> Self {
        Self { key }
    }
}

impl From<&str> for InternedString {
    fn from(name: &str) -> Self {
        Self {
            key: unsafe { INTERNER.get_or_intern(name) },
        }
    }
}

impl From<String> for InternedString {
    fn from(name: String) -> Self {
        Self {
            key: unsafe { INTERNER.get_or_intern(name) },
        }
    }
}

impl Debug for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InternedString({})", unsafe {
            INTERNER.resolve(&self.key)
        })
    }
}

impl Display for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", unsafe { INTERNER.resolve(&self.key) })
    }
}

impl Borrow<str> for InternedString {
    fn borrow(&self) -> &str {
        unsafe { INTERNER.resolve(&self.key) }
    }
}

impl Deref for InternedString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe { INTERNER.resolve(&self.key) }
    }
}

// #[derive(Debug, Clone, PartialEq)]
// pub struct Ident<T> {
//     name: T,
//     span: Span,
// }

// impl<T> Ident<T> {
//     pub fn new(name: T, span: Span) -> Self {
//         Self { name, span }
//     }

//     pub fn name(&self) -> &T {
//         &self.name
//     }

//     pub fn span(&self) -> &Span {
//         &self.span
//     }
// }
