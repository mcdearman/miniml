use super::{
    env::Env,
    error::{RuntimeError, RuntimeResult},
    value::{Lit, Value},
};
use crate::infer::tir::{self, DeclKind, Expr, ExprKind, Root};
use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub fn eval<'src>(src: &'src str, env: Rc<RefCell<Env>>, tir: Root) -> RuntimeResult<Value> {
    let mut types = HashMap::new();
    let mut val = Value::Unit;
    for decl in tir.decls() {
        match decl.kind() {
            DeclKind::DataType(dt) => {
                types.insert(*dt.name().id(), dt.kind().clone());
            }
            DeclKind::Let { name, expr, .. } => {
                val = eval_expr(src, env.clone(), expr.clone())?;
                env.borrow_mut().insert(*name.id(), val.clone());
            }
            DeclKind::Fn {
                name, params, expr, ..
            } => {
                let value = Value::Lambda {
                    env: env.clone(),
                    params: params.iter().map(|p| *p.id()).collect_vec(),
                    expr: expr.clone(),
                };
                env.borrow_mut().insert(*name.id(), value);
                if src[*name.span()].trim() == "main" {
                    val = eval_expr(src, env.clone(), expr.clone())?;
                }
            }
        }
    }
    Ok(val)
}

fn eval_expr<'src>(
    src: &'src str,
    mut env: Rc<RefCell<Env>>,
    mut expr: Expr,
) -> RuntimeResult<Value> {
    let val: Value;
    'tco: loop {
        val = match expr.kind().clone() {
            ExprKind::Lit(lit) => match lit {
                tir::Lit::Int(i) => Value::Lit(Lit::Int(i)),
                tir::Lit::Bool(b) => Value::Lit(Lit::Bool(b)),
                tir::Lit::String(s) => Value::Lit(Lit::String(s)),
            },
            ExprKind::Ident(name) => {
                if let Some(value) = env.borrow().get(name.id()) {
                    value
                } else {
                    return Err(RuntimeError::UnboundIdent(
                        src[*name.span()].trim().into(),
                        *name.span(),
                    ));
                }
            }
            ExprKind::Apply { fun, args } => match eval_expr(src, env.clone(), fun)? {
                Value::Lambda {
                    env: lam_env,
                    params,
                    expr: lam_expr,
                } => {
                    let vargs = args
                        .into_iter()
                        .map(|arg| eval_expr(src, env.clone(), arg))
                        .collect::<RuntimeResult<Vec<Value>>>()?;
                    let arg_env = Env::new_with_parent(lam_env.clone());
                    vargs.iter().zip(params.iter()).for_each(|(arg, p)| {
                        arg_env.borrow_mut().insert(*p, arg.clone());
                    });
                    expr = lam_expr;
                    env = arg_env;
                    continue 'tco;
                }
                Value::NativeFn(fun) => {
                    let mut new_args = Vec::new();
                    for arg in args {
                        new_args.push(eval_expr(src, env.clone(), arg)?);
                    }
                    fun(new_args)?
                }
                f => {
                    return Err(RuntimeError::TypeError(
                        format!("Expected lambda, found {:?}", f).into(),
                    ));
                }
            },
            ExprKind::Let {
                name,
                expr: let_expr,
                body,
            } => {
                let value = eval_expr(src, env.clone(), let_expr)?;
                let let_env = Env::new_with_parent(env.clone());
                let_env.borrow_mut().insert(*name.id(), value);
                expr = body;
                env = let_env;
                continue 'tco;
            }
            ExprKind::Fn {
                name,
                params,
                expr: fn_expr,
                body,
            } => {
                let lam_env = Env::new_with_parent(env.clone());
                let value = Value::Lambda {
                    env: lam_env.clone(),
                    params: params.iter().map(|p| *p.id()).collect_vec(),
                    expr: fn_expr,
                };
                env.borrow_mut().insert(*name.id(), value);
                expr = body;
                env = lam_env;
                continue 'tco;
            }
            ExprKind::If {
                cond, then, else_, ..
            } => {
                let cond = eval_expr(src, env.clone(), cond)?;
                match cond {
                    Value::Lit(Lit::Bool(true)) => {
                        expr = then;
                        continue 'tco;
                    }
                    Value::Lit(Lit::Bool(false)) => {
                        expr = else_;
                        continue 'tco;
                    }
                    _ => {
                        return Err(RuntimeError::TypeError(
                            format!("Expected bool, found {:?}", cond).into(),
                        ));
                    }
                }
            }
            ExprKind::Lambda {
                params,
                expr: lam_expr,
            } => Value::Lambda {
                env: Env::new_with_parent(env.clone()),
                params: params.iter().map(|p| *p.id()).collect_vec(),
                expr: lam_expr,
            },
            ExprKind::List(exprs) => {
                let mut list = Vec::new();
                for expr in exprs.iter() {
                    list.push(eval_expr(src, env.clone(), expr.clone())?);
                }
                Value::List(list.into())
            }
            ExprKind::Record { fields, .. } => {
                todo!()
                // let mut record = HashMap::new();
                // for (name, expr) in fields.iter() {
                //     record.insert(*name.id(), eval_expr(src, env.clone(), expr.clone())?);
                // }
                // Value::Record(record.into())
            }
            ExprKind::Unit => Value::Unit,
        };

        break 'tco;
    }
    Ok(val)
}
