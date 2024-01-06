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
