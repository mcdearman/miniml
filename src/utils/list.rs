use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Default)]
pub enum List<T> {
    #[default]
    Empty,
    Pair {
        head: Box<T>,
        tail: Box<Self>,
    },
}

impl<T> List<T> {
    pub fn head(&self) -> Option<&T> {
        match self {
            Self::Empty => None,
            Self::Pair { head, .. } => Some(head),
        }
    }

    pub fn tail(&self) -> Option<&Self> {
        match self {
            Self::Empty => None,
            Self::Pair { tail, .. } => Some(tail),
        }
    }

    pub fn push_front(&mut self, head: T) {
        let tail = std::mem::replace(self, Self::Empty);
        *self = Self::Pair {
            head: Box::new(head),
            tail: Box::new(tail),
        };
    }

    pub fn push_back(&mut self, head: T) {
        let mut tail = self;
        loop {
            match tail {
                Self::Empty => {
                    *tail = Self::Pair {
                        head: Box::new(head),
                        tail: Box::new(Self::Empty),
                    };
                    break;
                }
                Self::Pair { tail: next, .. } => {
                    tail = next;
                }
            }
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = &T> {
        ListIter::new(self)
    }
}

impl<'a, T> Display for List<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for (i, s) in self.iter().enumerate() {
            if i != 0 {
                write!(f, " ")?;
            }
            write!(f, "{}", s)?;
        }
        write!(f, "]")
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
struct ListIter<'a, T> {
    list: &'a List<T>,
}

impl<'a, T> ListIter<'a, T> {
    fn new(list: &'a List<T>) -> Self {
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
