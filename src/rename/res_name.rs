use std::sync::atomic::AtomicUsize;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ResName(usize);

static COUNTER: AtomicUsize = AtomicUsize::new(0);

impl ResName {
    pub fn new(id: usize) -> Self {
        Self(id)
    }

    pub fn gen() -> Self {
        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }
}
