use once_cell::sync::Lazy;

use crate::analysis::infer::meta;

use super::{
    error::{InferResult, TypeError},
    meta::{Meta, MetaId},
    r#type::Type,
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
        write!(f, "{:#08x}", self.0)
    }
}

impl Display for MetaRef {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:#08x}", self.0)
    }
}

impl From<MetaRef> for Meta {
    fn from(id: MetaRef) -> Self {
        unsafe { CTX.get(&id) }
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
        let id = MetaRef::gen();
        self.bindings.insert(id, Meta::fresh());
        id
    }

    pub fn get(&self, meta_ref: &MetaRef) -> Meta {
        self.bindings
            .get(meta_ref)
            .cloned()
            .expect("dangling meta reference")
    }

    pub fn bind(&mut self, meta_ref: &MetaRef, ty: &Type) -> InferResult<()> {
        match self.get(meta_ref) {
            Meta::Bound(t) => self.unify(&t, ty),
            Meta::Unbound(id) => {
                if *ty == Type::Meta(Box::new(Meta::Unbound(id))) {
                    Ok(())
                } else if ty.free_vars(self).contains(&id) {
                    Err(TypeError::from(format!(
                        "occurs check failed: {} occurs in {:?}",
                        id, ty
                    )))
                } else {
                    self.bindings.insert(*meta_ref, Meta::Bound(ty.clone()));
                    Ok(())
                }
            }
        }
    }

    pub fn force(&mut self, ty: &Type) -> Type {
        match ty {
            Type::MetaRef(id) => match self.clone().bindings.get(id) {
                Some(Meta::Bound(ty)) => {
                    let ty = self.force(ty);
                    self.bindings.insert(*id, Meta::Bound(ty.clone()));
                    ty
                }
                _ => ty.clone(),
            },
            _ => ty.clone(),
        }
    }

    pub fn unify(&mut self, t1: &Type, t2: &Type) -> InferResult<()> {
        log::debug!("unify: {:?} and {:?}", t1, t2);
        let t1 = self.force(t1);
        let t2 = self.force(t2);

        match (&t1, &t2) {
            (Type::Byte, Type::Byte)
            | (Type::Int, Type::Int)
            | (Type::Rational, Type::Rational)
            | (Type::Real, Type::Real)
            | (Type::Bool, Type::Bool)
            | (Type::String, Type::String)
            | (Type::Char, Type::Char)
            | (Type::Unit, Type::Unit) => Ok(()),
            (Type::Lambda(p1, b1), Type::Lambda(p2, b2)) => {
                if p1.len() != p2.len() {
                    return Err(TypeError::from(format!(
                        "functions have different arity: {:?} and {:?}",
                        t1, t2,
                    )));
                }
                for (t1, t2) in p1.iter().zip(p2.iter()) {
                    self.unify(t1, t2)?;
                }
                self.unify(b1, b2)
            }
            (Type::List(l1), Type::List(l2)) => self.unify(l1, l2),
            (t, Type::MetaRef(meta_ref)) => {
                self.bind(meta_ref, &t1)?;
                log::debug!("bind: {:?} to {:?}", meta_ref, t1);
                // log::debug!("meta_ctx: {:#?}", self);
                Ok(())
            }
            (Type::MetaRef(meta_ref), _) => {
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

// impl Debug for MetaContext {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {}
// }
