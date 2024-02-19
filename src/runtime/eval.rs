use super::{
    env::Env,
    error::{RuntimeError, RuntimeResult},
    value::{Lit, Value},
};
use crate::infer::tir::{self, DeclKind, Expr, ExprKind, Root};
use itertools::Itertools;

pub fn eval<'src>(src: &'src str, env: &mut Env, tir: Root) -> RuntimeResult<Value> {
    let mut val = Value::Unit;
    for decl in tir.decls() {
        match decl.kind() {
            DeclKind::Let { name, expr, .. } => {
                let value = eval_expr(src, env, expr.clone())?;
                env.def(*name.id(), value.clone());
                if src[*name.span()].trim() == "main" {
                    match value {
                        Value::Lambda { params, expr } => {
                            val = eval_expr(src, env, expr)?;
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(
                                format!("Expected lambda, found {:?}", value).into(),
                            ));
                        }
                    }
                } else {
                    val = Value::Unit;
                }
            }
        }
    }
    Ok(val)
}

fn eval_expr<'src>(src: &'src str, env: &mut Env, expr: Expr) -> RuntimeResult<Value> {
    // let val: Value;
    // 'tco: loop {
    match expr.kind().clone() {
        ExprKind::Lit(lit) => match lit {
            tir::Lit::Int(i) => Ok(Value::Lit(Lit::Int(i))),
            tir::Lit::Bool(b) => Ok(Value::Lit(Lit::Bool(b))),
        },
        ExprKind::Ident(name) => {
            if let Some(value) = env.get(name.id()) {
                Ok(value)
            } else {
                return Err(RuntimeError::UnboundIdent(src[*name.span()].trim().into(), *name.span()));
            }
        }
        ExprKind::Lambda { params, expr } => Ok(Value::Lambda {
            params: params.iter().map(|p| *p.id()).collect_vec(),
            expr,
        }),
        ExprKind::Apply { fun, args } => {
            match eval_expr(src, env, fun)? {
                Value::Lambda {
                    params,
                    expr: lam_expr,
                } => {
                    let vargs = args
                        .into_iter()
                        .map(|arg| eval_expr(src, env, arg))
                        .collect::<RuntimeResult<Vec<Value>>>()?;
                    env.push();
                    vargs.iter().zip(params.iter()).for_each(|(arg, p)| {
                        env.def(*p, arg.clone());
                    });
                    // println!("enva: {:#?}", env);
                    // expr = lam_expr;
                    // continue 'tco;
                    let val = eval_expr(src, env, lam_expr);
                    env.pop();
                    val
                }
                Value::NativeFn(fun) => {
                    let mut new_args = Vec::new();
                    for arg in args {
                        new_args.push(eval_expr(src, env, arg)?);
                    }
                    println!("new_args: {:#?}", new_args);
                    let val = fun(new_args);
                    // println!("val: {:?}", val);
                    val
                }
                f => {
                    return Err(RuntimeError::TypeError(
                        format!("Expected lambda, found {:?}", f).into(),
                    ));
                }
            }
        }
        ExprKind::Let {
            name,
            expr: let_expr,
            body,
        } => {
            let value = match let_expr.kind() {
                ExprKind::Lambda { params, expr } => {
                    env.def(
                        *name.id(),
                        Value::Lambda {
                            params: params.iter().map(|p| *p.id()).collect_vec(),
                            expr: expr.clone(),
                        },
                    );
                }
                _ => {}
            };
            let value = eval_expr(src, env, let_expr)?;
            env.push();
            env.def(*name.id(), value);
            let val = eval_expr(src, env, body);
            env.pop();
            val
        }
        ExprKind::If {
            cond, then, else_, ..
        } => {
            // println!("cond: {:?}", cond);
            let cond = eval_expr(src, env, cond)?;
            match cond {
                Value::Lit(Lit::Bool(true)) => {
                    // println!("then: {:?}", then);
                    // expr = then;
                    // continue 'tco;
                    eval_expr(src, env, then)
                }
                Value::Lit(Lit::Bool(false)) => {
                    // println!("else: {:?}", else_);
                    // expr = else_;
                    // continue 'tco;
                    eval_expr(src, env, else_)
                }
                _ => {
                    return Err(RuntimeError::TypeError(
                        format!("Expected bool, found {:?}", cond).into(),
                    ));
                }
            }
        }
        ExprKind::Unit => Ok(Value::Unit),
    }

    // break 'tco;
    // }
    // Ok(val)
}
