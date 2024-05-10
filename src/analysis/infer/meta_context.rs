use super::ty_var::TyVar;
use crate::utils::unique_id::UniqueId;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
pub struct TyVarContext {
    bindings: HashMap<UniqueId, TyVar>,
}
