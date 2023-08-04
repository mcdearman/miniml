use crate::{
    env::Env,
    eval::{eval, eval_expr},
    runtime_error::EvalResult,
    value::Value,
};
use miniml_syntax::ast::{Decl, Expr, Root};
use miniml_util::{intern::InternedString, span::Span};
use std::{cell::RefCell, rc::Rc};

pub fn exec(root: &Root) -> EvalResult<Value> {
    let env = Rc::new(RefCell::new(Env::new()));
    for d in root.decls.iter() {
        eval(env.clone(), &d.0)?;
    }
    let v = match env.borrow().find(&"main".into()) {
        Some(_) => eval_expr(
            env.clone(),
            &Expr::Apply {
                fun: Box::new(Expr::Ident(InternedString::from("main")).spanned(Span::from(0..0))),
                args: vec![],
            },
        )?,
        None => Value::Unit,
    };

    Ok(v)
}
