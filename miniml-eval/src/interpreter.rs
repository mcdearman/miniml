use crate::{
    env::Env,
    eval::{eval, eval_expr},
    runtime_error::EvalResult,
    value::Value,
};
use miniml_syntax::ast::{Decl, Expr, Root};
use miniml_util::{
    intern::InternedString,
    span::{Span, Spannable},
};
use std::{
    cell::{Ref, RefCell},
    rc::Rc,
};

fn default_env() -> Rc<RefCell<Env>> {
    let env = Rc::new(RefCell::new(Env::new()));
    env.borrow_mut().define(
        "print".into(),
        Value::Lambda {
            env: Env::with_parent(env.clone()),
            params: vec!["x".into()],
            body: Box::new(Expr::Apply {
                fun: Box::new(Expr::Ident(InternedString::from("print")).spanned(Span::from(0..0))),
                args: vec![Expr::Ident(InternedString::from("x")).spanned(Span::from(0..0))],
            }),
        },
    );
    env.borrow_mut().define(
        "println".into(),
        Value::Lambda {
            env: Env::with_parent(env.clone()),
            params: vec!["x".into()],
            body: Box::new(Expr::Apply {
                fun: Box::new(
                    Expr::Ident(InternedString::from("println")).spanned(Span::from(0..0)),
                ),
                args: vec![Expr::Ident(InternedString::from("x")).spanned(Span::from(0..0))],
            }),
        },
    );
    env
}

pub fn exec(root: &Root) -> EvalResult<Value> {
    let env = default_env();
    for d in root.decls.iter() {
        eval(env.clone(), &d.value)?;
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
