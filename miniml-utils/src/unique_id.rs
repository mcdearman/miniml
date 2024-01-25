use std::{
    fmt::{Debug, Display},
    sync::atomic::{AtomicUsize, Ordering},
};

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

impl Display for UniqueId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.0)
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
