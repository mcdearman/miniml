use super::{
    error::{InferResult, TypeError},
    meta::Meta,
    r#type::Type,
};
use crate::utils::unique_id::UniqueId;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct MetaContext {
    bindings: HashMap<UniqueId, Meta>,
}

impl MetaContext {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn fresh(&mut self) -> UniqueId {
        let key = UniqueId::gen();
        self.bindings.insert(key, Meta::fresh());
        key
    }

    pub fn insert(&mut self, meta: Meta) -> UniqueId {
        let id = UniqueId::gen();
        self.bindings.insert(id, meta);
        id
    }

    pub fn insert_or_update(&mut self, id: UniqueId, meta: Meta) {
        self.bindings.insert(id, meta);
    }

    pub fn get_mut(&mut self, id: UniqueId) -> Option<&mut Meta> {
        self.bindings.get_mut(&id)
    }

    pub fn bind(&mut self, key: &UniqueId, ty: &Type) -> InferResult<()> {
        let meta = self
            .bindings
            .get_mut(key)
            .ok_or(TypeError::from(format!("unbound meta variable: {}", key)))?;

        if *ty == Type::Meta(*key) {
            Ok(())
        } else if ty.free_vars().contains(key) {
            Err(TypeError::from(format!(
                "occurs check failed: {} occurs in {:?}",
                meta, ty
            )))
        } else {
            *meta = Meta::Bound(Box::new(*ty));
            Ok(())
        }
    }
}
