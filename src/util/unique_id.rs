use std::{
    fmt::Debug,
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
