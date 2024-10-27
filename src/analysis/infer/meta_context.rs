use once_cell::sync::Lazy;

use crate::analysis::infer::meta;

use super::{
    error::{InferResult, TypeError},
    meta::{Meta, MetaId},
    ty::Ty,
};
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

    pub fn bind(&mut self, meta_ref: &MetaRef, ty: &Ty) -> InferResult<()> {
        match self.get(meta_ref).ok_or(TypeError::from(format!(
            "unbound meta variable: {:?}",
            meta_ref
        )))? {
            Meta::Bound(t) => Err(TypeError::from(format!(
                "cannot bind {:?} to {:?}, already bound to {:?}",
                meta_ref, ty, t
            ))),
            m @ Meta::Unbound(id) => {
                if *ty == Ty::Meta(Box::new(Meta::Unbound(id))) {
                    Ok(())
                } else if ty.free_vars(self).contains(&id) {
                    Err(TypeError::from(format!(
                        "occurs check failed: {} occurs in {:?}",
                        m, ty
                    )))
                } else {
                    self.bindings.insert(*meta_ref, Meta::Bound(ty.clone()));
                    Ok(())
                }
            }
        }
    }

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

    pub fn unify(&mut self, t1: &Ty, t2: &Ty) -> InferResult<()> {
        log::debug!("unify: {:?} and {:?}", t1, t2);
        let t1 = self.force(t1);
        let t2 = self.force(t2);

        match (&t1, &t2) {
            (Ty::Byte, Ty::Byte)
            | (Ty::Int, Ty::Int)
            | (Ty::Rational, Ty::Rational)
            | (Ty::Real, Ty::Real)
            | (Ty::Bool, Ty::Bool)
            | (Ty::String, Ty::String)
            | (Ty::Char, Ty::Char)
            | (Ty::Unit, Ty::Unit) => Ok(()),
            (Ty::Lambda(p1, b1), Ty::Lambda(p2, b2)) => {
                self.unify(p1, p2)?;
                self.unify(b1, b2)
            }
            (Ty::List(l1), Ty::List(l2)) => self.unify(l1, l2),
            (_, Ty::MetaRef(meta_ref)) => {
                self.bind(meta_ref, &t1)?;
                log::debug!("bind: {:?} to {:?}", meta_ref, t1);
                // log::debug!("meta_ctx: {:#?}", self);
                Ok(())
            }
            (Ty::MetaRef(meta_ref), _) => {
                self.bind(meta_ref, &t2)?;
                log::debug!("bind: {:?} to {:?}", meta_ref, t2);
                // log::debug!("meta_ctx: {:#?}", self);
                Ok(())
            }
            // (Type::Poly(p1), Type::Poly(p2)) => todo!(),
            // (Type::Poly(p), _) => {
            //     let p = p.instantiate(self);
            //     self.unify(&p, &t2)
            // }
            // (_, Type::Poly(p)) => {
            //     let p = p.instantiate(self);
            //     self.unify(&t1, &p)
            // }
            _ => Err(TypeError::from(format!(
                "cannot unify {:?} and {:?}",
                t1, t2,
            ))),
        }
    }
}
