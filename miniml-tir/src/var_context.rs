use crate::{ty::Ty, ty_var::TyVar};
use once_cell::sync::Lazy;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    sync::{atomic::AtomicUsize, Mutex},
};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct VarId(usize);

impl VarId {
    pub fn gen() -> Self {
        Self(COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
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
    bindings: Mutex<HashMap<VarId, TyVar>>,
}

impl VarContext {
    pub fn new() -> Self {
        Self {
            counter: AtomicUsize::new(0),
            bindings: Mutex::new(HashMap::new()),
        }
    }

    pub fn fresh(&mut self) -> VarId {
        let r = VarId::gen();
        self.bindings.insert(r, TyVar::fresh());
        r
    }

    pub fn get(&self, var_id: &VarId) -> TyVar {
        self.bindings
            .get(var_id)
            .cloned()
            .expect("unbound meta ref")
    }

    pub fn insert(&mut self, var_id: VarId, meta: TyVar) {
        self.bindings.insert(var_id, meta);
    }

    pub fn force(&mut self, ty: &Ty) -> Ty {
        match ty {
            Ty::Var(id) => match self.get(id) {
                TyVar::Bound(ty) => {
                    let ty = self.force(&ty);
                    self.bindings.insert(*id, TyVar::Bound(ty.clone()));
                    ty
                }
                _ => ty.clone(),
            },
            _ => ty.clone(),
        }
    }
}
