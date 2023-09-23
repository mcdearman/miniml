use itertools::join;
use std::fmt::Display;

/// A singly-linked list with owned nodes.
#[derive(Debug, Clone, PartialEq)]
pub enum List<T> {
    Node(Box<(T, List<T>)>),
    Nil,
}

impl<T> Display for List<T>
where
    T: Display + Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            List::Node(_) => {
                write!(f, "[")?;
                write!(f, "{}", join(self.clone(), ", "))?;
                write!(f, "]")
            }
            List::Nil => write!(f, "[]"),
        }
    }
}

impl<T: Clone> Iterator for List<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            List::Node(n) => {
                let (data, next) = n.as_ref();
                let data = data.clone();
                *self = next.clone();
                Some(data)
            }
            List::Nil => None,
        }
    }
}

impl<T: Clone> ExactSizeIterator for List<T> {
    fn len(&self) -> usize {
        self.clone().fold(0, |acc, _| acc + 1)
    }
}

impl<T> FromIterator<T> for List<T> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        iter.into_iter()
            .fold(Self::Nil, |list, data| Self::Node(Box::new((data, list))))
    }
}
