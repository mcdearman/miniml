use super::{
    env::Env,
    error::{RuntimeError, RuntimeResult},
    value::{Lit, Record, Value},
};
use crate::{
    analysis::infer::tir::{self, *},
    utils::list::List,
};
use itertools::Itertools;
use std::{cell::RefCell, rc::Rc};

pub fn eval<'src>(src: &'src str, env: Rc<RefCell<Env>>, tir: Root) -> RuntimeResult<Value> {
    // let mut types = HashMap::new();
    let mut val = Value::Unit;
    for decl in &tir.decls {
        match &decl.kind {
            // DeclKind::DataType(dt) => {
            //     types.insert(dt.name().id(), dt.kind().clone());
            // }
            DeclKind::Let(name, expr) => {
                val = eval_expr(src, env.clone(), expr.clone())?;
                env.borrow_mut().insert(name.id, val.clone());
            }
            DeclKind::Fn(name, params, expr, ..) => {
                let value = Value::Lambda(
                    env.clone(),
                    params.iter().map(|p| p.id).collect_vec(),
                    expr.clone(),
                );
                env.borrow_mut().insert(name.id, value);
                if name.key.to_string() == "main" {
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
        val = match expr.kind.as_ref() {
            ExprKind::Lit(lit) => match lit {
                tir::Lit::Byte(b) => Value::Lit(Lit::Byte(*b)),
                tir::Lit::Int(i) => Value::Lit(Lit::Int(*i)),
                tir::Lit::Rational(r) => Value::Lit(Lit::Rational(*r)),
                tir::Lit::Real(r) => Value::Lit(Lit::Real(*r)),
                tir::Lit::Bool(b) => Value::Lit(Lit::Bool(*b)),
                tir::Lit::String(s) => Value::Lit(Lit::String(*s)),
                tir::Lit::Char(c) => Value::Lit(Lit::Char(*c)),
            },
            ExprKind::Var(name) => {
                if let Some(value) = env.borrow().get(&name.id) {
                    value
                } else {
                    return Err(RuntimeError::UnboundIdent(
                        src[name.span].trim().into(),
                        name.span,
                    ));
                }
            }
            ExprKind::Apply(fun, args) => match eval_expr(src, env.clone(), fun.clone())? {
                Value::Lambda(lam_env, params, lam_expr) => {
                    let vargs = args
                        .into_iter()
                        .map(|arg| eval_expr(src, env.clone(), arg.clone()))
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
                        new_args.push(eval_expr(src, env.clone(), arg.clone())?);
                    }
                    fun(new_args)?
                }
                f => {
                    return Err(RuntimeError::TypeError(
                        format!("Expected lambda, found {:?}", f).into(),
                    ));
                }
            },
            ExprKind::Or(lhs, rhs) => {
                if let Value::Lit(Lit::Bool(true)) = eval_expr(src, env.clone(), lhs.clone())? {
                    Value::Lit(Lit::Bool(true))
                } else {
                    eval_expr(src, env.clone(), rhs.clone())?
                }
            }
            ExprKind::And(lhs, rhs) => {
                if let Value::Lit(Lit::Bool(false)) = eval_expr(src, env.clone(), lhs.clone())? {
                    Value::Lit(Lit::Bool(false))
                } else {
                    eval_expr(src, env.clone(), rhs.clone())?
                }
            }
            ExprKind::Let(name, let_expr, body) => {
                let value = eval_expr(src, env.clone(), let_expr.clone())?;
                let let_env = Env::new_with_parent(env.clone());
                let_env.borrow_mut().insert(name.id, value);
                expr = body.clone();
                env = let_env;
                continue 'tco;
            }
            ExprKind::Fn(name, params, fn_expr, body) => {
                let lam_env = Env::new_with_parent(env.clone());
                let value = Value::Lambda(
                    lam_env.clone(),
                    params.iter().map(|p| p.id).collect_vec(),
                    fn_expr.clone(),
                );
                env.borrow_mut().insert(name.id, value);
                expr = body.clone();
                env = lam_env;
                continue 'tco;
            }
            ExprKind::If(cond, then, else_, ..) => {
                let cond = eval_expr(src, env.clone(), cond.clone())?;
                match cond {
                    Value::Lit(Lit::Bool(true)) => {
                        expr = then.clone();
                        continue 'tco;
                    }
                    Value::Lit(Lit::Bool(false)) => {
                        expr = else_.clone();
                        continue 'tco;
                    }
                    _ => {
                        return Err(RuntimeError::TypeError(
                            format!("Expected bool, found {:?}", cond).into(),
                        ));
                    }
                }
            }
            ExprKind::Lambda(params, lam_expr) => Value::Lambda(
                Env::new_with_parent(env.clone()),
                params.iter().map(|p| p.id).collect_vec(),
                lam_expr.clone(),
            ),
            // ExprKind::List(exprs) => {
            //     let mut list = Vec::new();
            //     for expr in exprs.iter() {
            //         list.push(eval_expr(src, env.clone(), expr.clone())?);
            //     }
            //     Value::List(list.into())
            // }
            // ExprKind::Record { name, fields } => {
            //     let mut record = vec![];
            //     for (name, expr) in fields.iter() {
            //         record.push((name.key(), eval_expr(src, env.clone(), expr.clone())?));
            //     }
            //     Value::Record(Record::new(name.id(), record))
            // }
            // ExprKind::Dot { expr, field } => {
            //     let record = eval_expr(src, env.clone(), expr)?;
            //     if let Value::Record(record) = record {
            //         if let Some(val) = record.get(&field.key()) {
            //             val.clone()
            //         } else {
            //             return Err(RuntimeError::UnboundIdent(
            //                 format!("Field {:?} not found", &src[field.span()]).into(),
            //                 field.span(),
            //             ));
            //         }
            //     } else {
            //         return Err(RuntimeError::TypeError(
            //             format!("Expected record, found {:?}", record).into(),
            //         ));
            //     }
            // }
            ExprKind::List(exprs) => {
                let mut list = Vec::new();
                for expr in exprs.iter() {
                    list.push(eval_expr(src, env.clone(), expr.clone())?);
                }
                Value::List(list.into())
            }
            ExprKind::Unit => Value::Unit,
        };

        break 'tco;
    }
    Ok(val)
}
