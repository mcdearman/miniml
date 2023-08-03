use crate::{
    env::{self, Env},
    runtime_error::EvalResult,
    value::Value,
};
use miniml_syntax::ast::{Decl, Expr};
use std::{cell::RefCell, rc::Rc};

pub fn eval(env: Rc<RefCell<Env>>, decl: &Decl) -> EvalResult<Value> {
    match decl.clone() {
        Decl::Const { name, expr } => {
            env.borrow_mut()
                .define(name.0, eval_expr(env.clone(), &expr.0)?);
            Ok(Value::Unit)
        }
        Decl::Let { name, expr } => todo!(),
        Decl::Fn { name, params, body } => todo!(),
    }
}

fn eval_expr(env: Rc<RefCell<Env>>, expr: &Expr) -> EvalResult<Value> {
    todo!()
}
