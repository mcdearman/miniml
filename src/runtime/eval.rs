use super::{
    env::Env,
    error::{RuntimeError, RuntimeResult},
    value::{Lit, Value},
};
use crate::infer::tir::{self, DeclKind, Expr, ExprKind, Root};
use itertools::Itertools;
use std::{cell::RefCell, rc::Rc};

pub fn eval<'src>(src: &'src str, env: Rc<RefCell<Env>>, tir: Root) -> RuntimeResult<Value> {
    let mut val = Value::Unit;
    for decl in tir.decls() {
        match decl.kind() {
            DeclKind::Let { name, expr, .. } => {
                match expr.kind() {
                    ExprKind::Lambda {
                        params,
                        expr: tir_expr,
                    } => {
                        let value = Value::Lambda {
                            env: env.clone(),
                            params: params.iter().map(|p| *p.id()).collect_vec(),
                            expr: tir_expr.clone(),
                        };
                        env.borrow_mut().insert(*name.id(), value);
                    }
                    _ => {
                        let value = eval_expr(src, env.clone(), expr.clone())?;
                        env.borrow_mut().insert(*name.id(), value);
                        // if src[*name.span()].trim() == "main" {
                        //     match value {
                        //         Value::Lambda {
                        //             env: lam_env,
                        //             params,
                        //             expr,
                        //         } => {
                        //             val = eval_expr(src, lam_env.clone(), expr.clone())?;
                        //         }
                        //         _ => {
                        //             return Err(RuntimeError::TypeError(
                        //                 format!("Expected lambda, found {:?}", value).into(),
                        //             ));
                        //         }
                        //     }
                        // } else {
                        //     val = Value::Unit;
                        // }
                    }
                }
                // let value = eval_expr(src, env.clone(), expr.clone())?;
                // env.borrow_mut().insert(*name.id(), value.clone());
                // if src[*name.span()].trim() == "main" {
                //     match value {
                //         Value::Lambda {
                //             env: lam_env,
                //             params,
                //             expr,
                //         } => {
                //             val = eval_expr(src, lam_env.clone(), expr)?;
                //         }
                //         _ => {
                //             return Err(RuntimeError::TypeError(
                //                 format!("Expected lambda, found {:?}", value).into(),
                //             ));
                //         }
                //     }
                // } else {
                //     val = Value::Unit;
                // }
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
            },
            ExprKind::Ident(name) => {
                if let Some(value) = env.borrow().get(name.id()) {
                    value
                } else {
                    // println!("env: {:#?}", env);
                    return Err(RuntimeError::UnboundIdent(
                        src[*name.span()].trim().into(),
                        *name.span(),
                    ));
                }
            }
            ExprKind::Lambda { params, expr } => Value::Lambda {
                env: Env::new_with_parent(env.clone()),
                params: params.iter().map(|p| *p.id()).collect_vec(),
                expr,
            },
            ExprKind::Apply { fun, args } => {
                match eval_expr(src, env.clone(), fun)? {
                    Value::Lambda {
                        env: lam_env,
                        params,
                        expr: lam_expr,
                    } => {
                        let vargs = args
                            .into_iter()
                            .map(|arg| eval_expr(src, env.clone(), arg))
                            .collect::<RuntimeResult<Vec<Value>>>()?;
                        // let arg_env = Env::new_with_parent(lam_env.clone());
                        vargs.iter().zip(params.iter()).for_each(|(arg, p)| {
                            lam_env.borrow_mut().insert(*p, arg.clone());
                        });
                        expr = lam_expr;
                        env = lam_env;
                        continue 'tco;
                    }
                    Value::NativeFn(fun) => {
                        let mut new_args = Vec::new();
                        for arg in args {
                            new_args.push(eval_expr(src, env.clone(), arg)?);
                        }
                        // println!("new_args: {:#?}", new_args);
                        fun(new_args)?
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
            } => match let_expr.kind() {
                ExprKind::Lambda {
                    params,
                    expr: tir_expr,
                } => {
                    // println!("lambda: {:#?}", let_expr);
                    let let_env = Env::new_with_parent(env.clone());
                    let value = Value::Lambda {
                        env: Env::new_with_parent(let_env.clone()),
                        params: params.iter().map(|p| *p.id()).collect_vec(),
                        expr: tir_expr.clone(),
                    };
                    let_env.borrow_mut().insert(*name.id(), value);
                    println!("let_env: {:#?}", let_env);
                    // eval_expr(src, let_env.clone(), body)?
                    expr = body;
                    env = let_env;
                    continue 'tco;
                }
                _ => {
                    let value = eval_expr(src, env.clone(), let_expr)?;
                    let let_env = Env::new_with_parent(env.clone());
                    let_env.borrow_mut().insert(*name.id(), value);
                    // eval_expr(src, let_env.clone(), body)?
                    expr = body;
                    env = let_env;
                    continue 'tco;
                }
            },
            ExprKind::If {
                cond, then, else_, ..
            } => {
                // println!("cond: {:?}", cond);
                let cond = eval_expr(src, env.clone(), cond)?;
                match cond {
                    Value::Lit(Lit::Bool(true)) => {
                        // println!("then: {:?}", then);
                        expr = then;
                        continue 'tco;
                        // eval_expr(src, env, then)
                    }
                    Value::Lit(Lit::Bool(false)) => {
                        // println!("else: {:?}", else_);
                        expr = else_;
                        continue 'tco;
                        // eval_expr(src, env, else_)
                    }
                    _ => {
                        return Err(RuntimeError::TypeError(
                            format!("Expected bool, found {:?}", cond).into(),
                        ));
                    }
                }
            }
            ExprKind::Unit => Value::Unit,
        };

        break 'tco;
    }
    Ok(val)
}
