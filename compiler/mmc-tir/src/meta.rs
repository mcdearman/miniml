use super::ty::Ty;
use once_cell::sync::Lazy;
use std::sync::atomic::AtomicU32;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    sync::{atomic::AtomicUsize, Arc, Mutex},
};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Meta {
    Bound(Ty),
    Unbound(u32),
}

static COUNTER: AtomicU32 = AtomicU32::new(0);

impl Meta {
    pub fn new(ty: Ty) -> Self {
        Self::Bound(ty)
    }

    pub fn fresh() -> Self {
        Self::Unbound(COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }

    pub fn id(&self) -> Option<u32> {
        match self {
            Self::Bound(_) => None,
            Self::Unbound(id) => Some(*id),
        }
    }
}

impl Debug for Meta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

#[rustfmt::skip]
const ALPHABET: &[char] = &[
    'a', 'b', 'c', 'd', 'e', 'f', 
    'g', 'h', 'i', 'j', 'k', 'l', 
    'm', 'n', 'o', 'p', 'q', 'r', 
    's', 't', 'u', 'v', 'w', 'x', 
    'y', 'z',
];

impl Display for Meta {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bound(ty) => write!(f, "{:?}", ty),
            Self::Unbound(id) => {
                let id = *id as usize;
                if id < ALPHABET.len() {
                    write!(f, "{}", ALPHABET[id])
                } else {
                    write!(
                        f,
                        "{}{}",
                        ALPHABET[id / ALPHABET.len() - 1],
                        (id + 1) % ALPHABET.len()
                    )
                }
            }
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MetaId(usize);

impl MetaId {
    pub fn fresh() -> Self {
        CTX.fresh()
    }

    pub fn get(&self) -> Meta {
        CTX.get(self)
    }

    pub fn insert(&self, meta: Meta) {
        CTX.insert(*self, meta);
    }
}

impl Debug for MetaId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "m{}", self.0)
    }
}

impl Display for MetaId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "m{}", self.0)
    }
}

impl From<MetaId> for Meta {
    fn from(id: MetaId) -> Self {
        CTX.get(&id)
    }
}

static CTX: Lazy<MetaContext> = Lazy::new(|| MetaContext::new());

#[derive(Debug)]
struct MetaContext {
    counter: AtomicUsize,
    bindings: Arc<Mutex<HashMap<MetaId, Meta>>>,
}

impl MetaContext {
    fn new() -> Self {
        Self {
            counter: AtomicUsize::new(0),
            bindings: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    fn fresh(&self) -> MetaId {
        let r = MetaId(
            self.counter
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        );
        self.bindings.lock().unwrap().insert(r, Meta::fresh());
        r
    }

    fn get(&self, meta_id: &MetaId) -> Meta {
        self.bindings
            .lock()
            .unwrap()
            .get(meta_id)
            .cloned()
            .expect("unbound meta ref")
    }

    fn insert(&self, meta_id: MetaId, meta: Meta) {
        self.bindings.lock().unwrap().insert(meta_id, meta);
    }
}
