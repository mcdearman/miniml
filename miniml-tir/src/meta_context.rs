use crate::{meta::Meta, ty::Ty};
use once_cell::sync::Lazy;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    sync::atomic::AtomicUsize,
};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MetaRef(usize);

impl MetaRef {
    pub fn gen() -> Self {
        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }
}

impl Debug for MetaRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "m{}", self.0)
    }
}

impl Display for MetaRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "m{}", self.0)
    }
}

impl From<MetaRef> for Meta {
    fn from(id: MetaRef) -> Self {
        unsafe { CTX.get(&id).expect("unbound meta ref") }
    }
}

static COUNTER: AtomicUsize = AtomicUsize::new(0);
pub static mut CTX: Lazy<MetaContext> = Lazy::new(|| MetaContext::new());

#[derive(Debug, Clone, PartialEq)]
pub struct MetaContext {
    bindings: HashMap<MetaRef, Meta>,
}

impl MetaContext {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn fresh(&mut self) -> MetaRef {
        let r = MetaRef::gen();
        self.bindings.insert(r, Meta::fresh());
        r
    }

    pub fn get(&self, meta_ref: &MetaRef) -> Option<Meta> {
        self.bindings.get(meta_ref).cloned()
    }

    pub fn insert(&mut self, meta_ref: MetaRef, meta: Meta) {}

    pub fn force(&mut self, ty: &Ty) -> Ty {
        match ty {
            Ty::MetaRef(id) => match self.get(id) {
                Some(Meta::Bound(ty)) => {
                    let ty = self.force(&ty);
                    self.bindings.insert(*id, Meta::Bound(ty.clone()));
                    ty
                }
                _ => ty.clone(),
            },
            _ => ty.clone(),
        }
    }
}
