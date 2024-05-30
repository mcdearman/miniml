use super::{
    error::{InferResult, TypeError},
    meta::Meta,
    r#type::Type,
};
use crate::utils::unique_id::UniqueId;
use std::{collections::HashMap, fmt::Debug};

#[derive(Clone, PartialEq)]
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
                for (meta_id, meta_ty) in self.clone().bindings {
                    if meta_ty == Type::Meta(Meta::new(id)) {
                        self.bindings.insert(meta_id, ty.clone());
                    }
                }
                self.bindings.insert(id, ty);
            }
        }
    }

    pub fn get(&self, id: &UniqueId) -> Option<&Type> {
        self.bindings.get(id)
    }

    pub fn bind(&mut self, meta: &Meta, ty: &Type) -> InferResult<()> {
        if *ty == Type::Meta(meta.clone()) {
            Ok(())
        } else if let Some(binding) = self.bindings.get_mut(&meta.id()) {
            if ty.free_vars().contains(meta) {
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

impl Debug for MetaContext {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = f.debug_map();
        for (id, ty) in self.bindings.iter() {
            builder.entry(&Meta::new(*id), ty);
        }
        builder.finish()
    }
}
