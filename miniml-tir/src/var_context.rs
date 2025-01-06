use crate::{ty::Ty, ty_var::TyVar};
use once_cell::sync::Lazy;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    sync::{atomic::AtomicUsize, Arc, Mutex},
};

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
pub struct VarContext {
    counter: AtomicUsize,
    bindings: Arc<Mutex<HashMap<VarId, TyVar>>>,
}

impl VarContext {
    pub fn new() -> Self {
        Self {
            counter: AtomicUsize::new(0),
            bindings: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    pub fn fresh(&self) -> VarId {
        let r = VarId(
            self.counter
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        );
        self.bindings.lock().unwrap().insert(r, TyVar::fresh());
        r
    }

    pub fn get(&self, var_id: &VarId) -> TyVar {
        self.bindings
            .lock()
            .unwrap()
            .get(var_id)
            .cloned()
            .expect("unbound meta ref")
    }

    pub fn insert(&self, var_id: VarId, meta: TyVar) {
        self.bindings.lock().unwrap().insert(var_id, meta);
    }
}
