use self::{env::Env, error::Error, nir::*};
use crate::parse::ast;
use std::{cell::RefCell, rc::Rc};

pub mod env;
pub mod error;
pub mod nir;

pub fn resolve(env: Rc<RefCell<Env>>, ast: &ast::Root) -> (Option<Root>, Vec<Error>) {
    todo!()
}
