use super::{
    env::Env,
    error::{RuntimeError, RuntimeResult},
    value::{Lit, Record, Value},
};
use crate::{
    analysis::infer::{
        r#type::Type,
        tir::{self, *},
    },
    utils::{intern::InternedString, list::List},
};
use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone)]
pub enum RuntimePayload {
    Type(Type),
    Bindings(Vec<(InternedString, Value, Type)>),
    Value(Value, Type),
}

pub fn eval<'src>(
    src: &'src str,
    env: Rc<RefCell<Env>>,
    tir: Root,
) -> RuntimeResult<RuntimePayload> {
    // let mut types = HashMap::new();
    let mut val = Value::Unit;
    let mut ty = Type::Unit;
    let mut name = None;
    for decl in &tir.decls {
        match &decl.kind {
            // DeclKind::DataType(dt) => {
            //     types.insert(dt.name().id(), dt.kind().clone());
            // }
            DeclKind::Let(pat, expr) => {
                val = eval_expr(src, env.clone(), expr.clone())?;
                if !destructure_pattern(src, env.clone(), pat, &val) {
                    return Err(RuntimeError::PatternMismatch(
                        format!("let decl{:?}", pat).into(),
                        pat.span,
                    ));
                }
                ty = expr.ty.clone();
            }
            DeclKind::Fn(name, params, expr, ..) => {
                let value = Value::Lambda(env.clone(), params.clone(), expr.clone());
                env.borrow_mut().insert(name.id, value);
                if name.key.to_string() == "main" {
                    val = eval_expr(src, env.clone(), expr.clone())?;
                    ty = expr.ty.clone();
                } else {
                    val = Value::Unit;
                    ty = decl.ty.clone();
                }
            }
        }
    }
    Ok((val, ty))
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
                    for (arg, p) in vargs.iter().zip(params.iter()) {
                        // arg_env.borrow_mut().insert(*p, arg.clone());
                        if !destructure_pattern(src, arg_env.clone(), p, arg).0 {
                            return Err(RuntimeError::PatternMismatch(
                                format!("apply {:?}", p).into(),
                                p.span,
                            ));
                        }
                    }
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
            ExprKind::Let(pat, let_expr, body) => {
                let value = eval_expr(src, env.clone(), let_expr.clone())?;
                let let_env = Env::new_with_parent(env.clone());
                // let_env.borrow_mut().insert(name.id, value);
                if !destructure_pattern(src, let_env.clone(), pat, &value).0 {
                    return Err(RuntimeError::PatternMismatch(
                        format!("let {:?}", pat).into(),
                        pat.span,
                    ));
                }
                expr = body.clone();
                env = let_env;
                continue 'tco;
            }
            ExprKind::Fn(name, params, fn_expr, body) => {
                let lam_env = Env::new_with_parent(env.clone());
                let value = Value::Lambda(lam_env.clone(), params.clone(), fn_expr.clone());
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
            ExprKind::Match(match_expr, arms) => {
                let match_val = eval_expr(src, env.clone(), match_expr.clone())?;
                for (pat, arm_expr) in arms.iter() {
                    let arm_env = Env::new_with_parent(env.clone());
                    if destructure_pattern(src, arm_env.clone(), pat, &match_val).0 {
                        expr = arm_expr.clone();
                        env = arm_env;
                        continue 'tco;
                    }
                }
                return Err(RuntimeError::PatternMismatch(
                    format!("match {:?}", match_val).into(),
                    match_expr.span,
                ));
            }
            ExprKind::Lambda(params, lam_expr) => Value::Lambda(
                Env::new_with_parent(env.clone()),
                params.clone(),
                lam_expr.clone(),
            ),
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

fn destructure_pattern(
    src: &str,
    env: Rc<RefCell<Env>>,
    pat: &tir::Pattern,
    val: &Value,
) -> (bool, HashMap<InternedString, Value>) {
    let mut bindings = HashMap::new();

    match pat.kind.as_ref() {
        PatternKind::Wildcard => (true, bindings.clone()),
        PatternKind::Lit(lit) => {
            if let Value::Lit(l) = val {
                if lit == l {
                    return (true, bindings);
                }
            }
            return (false, bindings.clone());
        }
        PatternKind::Ident(ident) => {
            env.borrow_mut().insert(ident.id, val.clone());
            bindings.insert(ident.key, val.clone());
            (true, bindings.clone())
        }
        PatternKind::List(list) => {
            if let Value::List(vals) = val {
                if list.len() != vals.len() {
                    return (false, bindings);
                }
                let mut ret = HashMap::new();
                for (pat, val) in list.iter().zip(vals.iter()) {
                    let d = destructure_pattern(src, env.clone(), pat, val);
                    if !d.0 {
                        return (false, None);
                    }
                }
                return true;
            }
            false
        }
        PatternKind::Pair(head, tail) => {
            if let Value::List(vals) = val {
                (
                    destructure_pattern(
                        src,
                        env.clone(),
                        head,
                        if let Some(head) = vals.head() {
                            head
                        } else {
                            return false;
                        },
                    ) && destructure_pattern(
                        src,
                        env.clone(),
                        tail,
                        &match vals.tail() {
                            Some(tail) => Value::List(tail.clone()),
                            None => {
                                return false;
                            }
                        },
                    ),
                    None,
                )
            } else {
                (false, None)
            }
        }
        PatternKind::Unit => {
            if let Value::Unit = val {
                return (true, None);
            }
            (false, None)
        }
    }
}
