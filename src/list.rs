use std::fmt::Display;

use itertools::join;

/// A singly-linked list with owned nodes.
#[derive(Debug, Clone, PartialEq)]
pub enum List<T> {
    Node(T, Box<List<T>>),
    Nil,
}

impl<T> Display for List<T>
where
    T: Display + Clone,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            List::Node(data, next) => {
                write!(f, "[")?;
                write!(f, "{}", join(self.clone(), ", "))?;
                write!(f, "]")
            }
            List::Nil => write!(f, "[]"),
        }
    }
}

impl<T> Iterator for List<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.head.take() {
            Some(node) => {
                self.head = node.next;
                Some(node.data)
            }
            None => None,
        }

        match self {
            List::Node(data, next) => {
                let data = data.clone();
                *self = *next.clone();
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
        iter.into_iter().fold(Self::NIL, |list, data| {
            Self::new(Some(Box::new(Node {
                data,
                next: list.head,
            })))
        })
    }
}

impl<T> From<Vec<T>> for List<T> {
    fn from(v: Vec<T>) -> Self {
        v.into_iter().rev().fold(Self::NIL, |list, data| {
            Self::new(Some(Box::new(Node {
                data,
                next: list.head,
            })))
        })
    }
}
