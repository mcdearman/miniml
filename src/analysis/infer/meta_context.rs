use super::{
    error::{InferResult, TypeError},
    meta::Meta,
    r#type::Type,
};
use crate::utils::unique_id::UniqueId;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct MetaContext {
    bindings: HashMap<UniqueId, Type>,
}

impl MetaContext {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub fn insert(&mut self, id: UniqueId, ty: Type) {
        match &ty {
            Type::Meta(meta) => {
                if let Some(ty) = self.bindings.get(&meta.id()) {
                    self.bindings.insert(id, ty.clone());
                } else {
                    self.bindings.insert(id, ty);
                }
            }
            _ => {
                self.bindings.insert(id, ty);
            }
        }
    }

    pub fn get(&self, id: &UniqueId) -> Option<&Type> {
        self.bindings.get(id)
    }

    pub fn bind(&mut self, meta: &Meta, ty: &Type) -> InferResult<()> {
        if let Some(binding) = self.bindings.get_mut(&meta.id()) {
            if *ty == Type::Meta(meta.clone()) {
                Ok(())
            } else if ty.free_vars().contains(meta) {
                Err(TypeError::from(format!(
                    "occurs check failed: {} occurs in {:?}",
                    meta, ty
                )))
            } else {
                *binding = ty.clone();
                Ok(())
            }
        } else {
            self.insert(meta.id(), ty.clone());
            Ok(())
        }
    }
}
