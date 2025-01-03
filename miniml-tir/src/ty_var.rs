use super::ty::Ty;
use std::{
    fmt::{Debug, Display},
    sync::atomic::AtomicU32,
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
