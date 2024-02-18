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

fn eval_expr<'src>(src: &'src str, env: &mut Env, mut expr: Expr) -> RuntimeResult<Value> {
    let val: Value;
    'tco: loop {
        val = match expr.kind().clone() {
            ExprKind::Lit(lit) => match lit {
                tir::Lit::Int(i) => Value::Lit(Lit::Int(i)),
                tir::Lit::Bool(b) => Value::Lit(Lit::Bool(b)),
            },
            ExprKind::Ident(name) => {
                if let Some(value) = env.get(name.id()) {
                    value
                } else {
                    return Err(RuntimeError::UnboundIdent(src[*name.span()].trim().into()));
                }
            }
            ExprKind::Lambda { params, expr } => Value::Lambda {
                params: params.iter().map(|p| *p.id()).collect_vec(),
                expr,
            },
            ExprKind::Apply { fun, args } => {
                let fun = eval_expr(src, env, fun)?;
                // println!("fun: {:?}", fun);
                match fun {
                    Value::Lambda {
                        params,
                        expr: lam_expr,
                    } => {
                        for (&p, arg) in params.iter().zip(args) {
                            let arg = eval_expr(src, env, arg)?;
                            env.def(p, arg);
                        }
                        expr = lam_expr;
                        continue 'tco;
                    }
                    Value::NativeFn(fun) => {
                        let mut new_args = Vec::new();
                        for arg in args {
                            new_args.push(eval_expr(src, env, arg)?);
                        }
                        fun(new_args)?
                    }
                    _ => {
                        return Err(RuntimeError::TypeError(
                            format!("Expected lambda, found {:?}", fun).into(),
                        ));
                    }
                }
            }
            ExprKind::Let {
                name,
                expr: let_expr,
                body,
            } => {
                let value = eval_expr(src, env, let_expr)?;
                env.def(*name.id(), value);
                let val = eval_expr(src, env, body)?;
                env.del(name.id());
                val
            }
            ExprKind::If {
                cond, then, else_, ..
            } => {
                let cond = eval_expr(src, env, cond)?;
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
            ExprKind::Unit => Value::Unit,
        };

        break 'tco;
    }
    Ok(val)
}
