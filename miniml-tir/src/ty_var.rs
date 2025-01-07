use super::ty::Ty;
use once_cell::sync::Lazy;
use std::sync::atomic::AtomicU32;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    sync::{atomic::AtomicUsize, Arc, Mutex},
};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TyVar {
    Bound(Ty),
    Unbound(u32),
}

static COUNTER: AtomicU32 = AtomicU32::new(0);

impl TyVar {
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

impl Debug for TyVar {
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

impl Display for TyVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bound(ty) => write!(f, "{:?}", ty),
            Self::Unbound(id) => {
                let id = u32::from(*id) as usize;
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
pub struct VarId(usize);

impl VarId {
    pub fn fresh() -> Self {
        CTX.fresh()
    }

    pub fn get(&self) -> TyVar {
        CTX.get(self)
    }

    pub fn insert(&self, meta: TyVar) {
        CTX.insert(*self, meta);
    }
}

impl Debug for VarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "m{}", self.0)
    }
}

impl Display for VarId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "m{}", self.0)
    }
}

impl From<VarId> for TyVar {
    fn from(id: VarId) -> Self {
        CTX.get(&id)
    }
}

static CTX: Lazy<VarContext> = Lazy::new(|| VarContext::new());

#[derive(Debug)]
struct VarContext {
    counter: AtomicUsize,
    bindings: Arc<Mutex<HashMap<VarId, TyVar>>>,
}

impl VarContext {
    fn new() -> Self {
        Self {
            counter: AtomicUsize::new(0),
            bindings: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    fn fresh(&self) -> VarId {
        let r = VarId(
            self.counter
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        );
        self.bindings.lock().unwrap().insert(r, TyVar::fresh());
        r
    }

    fn get(&self, var_id: &VarId) -> TyVar {
        self.bindings
            .lock()
            .unwrap()
            .get(var_id)
            .cloned()
            .expect("unbound meta ref")
    }

    fn insert(&self, var_id: VarId, meta: TyVar) {
        self.bindings.lock().unwrap().insert(var_id, meta);
    }
}
