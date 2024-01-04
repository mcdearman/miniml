use std::fmt::Debug;

use crate::{
    interpreter::{Interpreter, RuntimeResult},
    object::Object,
};

#[derive(Clone, PartialEq)]
pub struct NativeFn {
    name: String,
    arity: u8,
    callable: fn(&mut Interpreter, &[Object]) -> RuntimeResult<Object>,
}

impl NativeFn {
    pub fn new(
        name: String,
        arity: u8,
        callable: fn(&mut Interpreter, &[Object]) -> RuntimeResult<Object>,
    ) -> Self {
        Self {
            name,
            arity,
            callable,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn arity(&self) -> u8 {
        self.arity
    }

    pub fn call(&self, interp: &mut Interpreter, args: &[Object]) -> RuntimeResult<Object> {
        (self.callable)(interp, args)
    }
}

impl Debug for NativeFn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<NativeFn({})>", self.name)
    }
}
