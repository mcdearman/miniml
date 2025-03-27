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

    pub fn len(&self) -> usize {
        let mut len = 0;
        let mut list = self;
        loop {
            match list {
                Self::Empty => break,
                Self::Pair { tail, .. } => {
                    len += 1;
                    list = tail;
                }
            }
        }
        len
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

    pub fn iter(&self) -> ListIterRef<'_, T> {
        ListIterRef::new(self)
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
                write!(f, ", ")?;
            }
            write!(f, "{}", s)?;
        }
        write!(f, "]")
    }
}

impl<T> IntoIterator for List<T> {
    type Item = T;
    type IntoIter = ListIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        ListIter::new(self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ListIter<T> {
    list: List<T>,
}

impl<T> ListIter<T> {
    fn new(list: List<T>) -> Self {
        Self { list }
    }
}

impl<T> Iterator for ListIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match std::mem::replace(&mut self.list, List::Empty) {
            List::Empty => None,
            List::Pair { head, tail } => {
                self.list = *tail;
                Some(*head)
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ListIterRef<'a, T> {
    list: &'a List<T>,
}

impl<'a, T> ListIterRef<'a, T> {
    pub fn new(list: &'a List<T>) -> Self {
        Self { list }
    }
}

impl<'a, T> Iterator for ListIterRef<'a, T> {
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
