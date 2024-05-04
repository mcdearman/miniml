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
use libc::bind;
use std::{cell::RefCell, collections::HashMap, hash::Hash, rc::Rc};

#[derive(Debug, Clone)]
pub enum RuntimePayload {
    Type(Type),
    Bindings(Vec<(InternedString, Value, Type)>),
    Value(Value, Type),
}

impl Default for RuntimePayload {
    fn default() -> Self {
        Self::Value(Value::Unit, Type::Unit)
    }
}

pub fn eval<'src>(
    src: &'src str,
    env: Rc<RefCell<Env>>,
    tir: Root,
) -> RuntimeResult<RuntimePayload> {
    // let mut types = HashMap::new();
    let mut payload = RuntimePayload::default();

    for decl in &tir.decls {
        match &decl.kind {
            // DeclKind::DataType(dt) => {
            //     types.insert(dt.name().id(), dt.kind().clone());
            // }
            DeclKind::Let(pat, expr) => {
                let val = eval_expr(src, env.clone(), expr.clone())?;
                let (matched, bindings) = destructure_pattern(src, env.clone(), pat, &val);
                if !matched {
                    return Err(RuntimeError::PatternMismatch(
                        format!("let decl{:?}", pat).into(),
                        pat.span,
                    ));
                }
                let ty = expr.ty.clone();
                payload = RuntimePayload::Bindings(
                    bindings
                        .into_iter()
                        .map(|(ident, value)| (ident, value, ty.clone()))
                        .collect(),
                );
            }
            DeclKind::Fn(ident, params, expr, ..) => {
                let value = Value::Lambda(env.clone(), params.clone(), expr.clone());
                env.borrow_mut().insert(ident.id, value);
                if ident.key.to_string() == "main" {
                    return Ok(RuntimePayload::Value(
                        eval_expr(src, env.clone(), expr.clone())?,
                        expr.ty.clone(),
                    ));
                } else {
                    payload = RuntimePayload::default();
                }
            }
        }
    }
    Ok(payload)
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
            ExprKind::Var(ident) => {
                if let Some(value) = env.borrow().get(&ident.id) {
                    value
                } else {
                    return Err(RuntimeError::UnboundIdent(
                        src[ident.span].trim().into(),
                        ident.span,
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
            ExprKind::Fn(ident, params, fn_expr, body) => {
                let lam_env = Env::new_with_parent(env.clone());
                let value = Value::Lambda(lam_env.clone(), params.clone(), fn_expr.clone());
                env.borrow_mut().insert(ident.id, value);
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
    fn inner(
        src: &str,
        env: Rc<RefCell<Env>>,
        pat: &tir::Pattern,
        val: &Value,
        bindings: &mut HashMap<InternedString, Value>,
    ) -> (bool, HashMap<InternedString, Value>) {
        match pat.kind.as_ref() {
            PatternKind::Wildcard => (true, bindings.clone()),
            PatternKind::Lit(lit) => (
                match val {
                    Value::Lit(l) => lit == l,
                    _ => false,
                },
                bindings.clone(),
            ),
            PatternKind::Ident(ident) => {
                env.borrow_mut().insert(ident.id, val.clone());
                log::debug!("inserted {:?} -> {:?}", ident, val);
                bindings.insert(ident.key, val.clone());
                (true, bindings.clone())
            }
            PatternKind::List(list) => match val {
                Value::List(vals) => {
                    if list.len() != vals.len() {
                        return (false, bindings.clone());
                    }
                    for (pat, val) in list.iter().zip(vals.iter()) {
                        let (res, new_bindings) = inner(src, env.clone(), pat, val, bindings);
                        if !res {
                            return (false, bindings.clone());
                        }
                        bindings.extend(new_bindings.clone())
                    }
                    (true, bindings.clone())
                }
                _ => (false, bindings.clone()),
            },
            PatternKind::Pair(head, tail) => match val {
                Value::List(vals) => {
                    // vals.head()
                    //     .map(|head_val| destructure_pattern(src, env.clone(), head, head_val))
                    //     .is_some()
                    //     && vals
                    //         .tail()
                    //         .map(|tail_val| {
                    //             destructure_pattern(
                    //                 src,
                    //                 env.clone(),
                    //                 tail,
                    //                 &Value::List(tail_val.clone()),
                    //             )
                    //         })
                    //         .is_some()
                    if let Some(val_head) = vals.head() {
                        let (res_head, new_bindings_head) =
                            inner(src, env.clone(), head, val_head, bindings);
                        if !res_head {
                            return (false, bindings.clone());
                        }
                        bindings.extend(new_bindings_head.clone());
                    }
                    if let Some(val_tail) = vals.tail() {
                        let (res_tail, new_bindings_tail) = inner(
                            src,
                            env.clone(),
                            tail,
                            &Value::List(val_tail.clone()),
                            bindings,
                        );
                        if !res_tail {
                            return (false, bindings.clone());
                        }
                        bindings.extend(new_bindings_tail.clone());
                    }
                    (true, bindings.clone())
                }
                _ => (false, bindings.clone()),
            },
            PatternKind::Unit => (
                match val {
                    Value::Unit => true,
                    _ => false,
                },
                bindings.clone(),
            ),
        }
    }

    let mut bindings = HashMap::new();
    inner(src, env, pat, val, &mut bindings)
}
