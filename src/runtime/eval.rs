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
            DeclKind::Def(pat, expr) => {
                match expr.kind.as_ref() {
                    ExprKind::Lambda(params, lam_expr) => {
                        let lam_env = Env::new_with_parent(env.clone());
                        let value =
                            Value::Lambda(lam_env.clone(), params.clone(), lam_expr.clone());
                        if let PatternKind::Ident(ident) = pat.kind.as_ref() {
                            if ident.name.to_string() == "main" {
                                return Ok(RuntimePayload::Value(
                                    eval_expr(src, lam_env.clone(), lam_expr.clone())?,
                                    lam_expr.ty.clone(),
                                ));
                            }
                            env.borrow_mut().insert(ident.name, value.clone());
                            payload =
                                RuntimePayload::Bindings(vec![(ident.name, value, pat.ty.clone())]);
                            // log::debug!("payload: {:#?}", payload);
                        } else {
                            return Err(RuntimeError::PatternMismatch(
                                format!("def {:?}", pat).into(),
                                pat.span,
                            ));
                        }
                    }
                    _ => {
                        let val = eval_expr(src, env.clone(), expr.clone())?;
                        let (matched, bindings) = destructure_pattern(src, env.clone(), pat, &val);
                        if !matched {
                            return Err(RuntimeError::PatternMismatch(
                                format!("def {:?}", pat).into(),
                                pat.span,
                            ));
                        }
                        payload = RuntimePayload::Bindings(bindings);
                    }
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
                if let Some(value) = env.borrow().get(&ident.name) {
                    value
                } else {
                    return Err(RuntimeError::UnboundIdent(ident.name, ident.span));
                }
            }
            ExprKind::Apply(fun, arg) => match eval_expr(src, env.clone(), fun.clone())? {
                Value::Lambda(lam_env, param, lam_expr) => {
                    let varg = eval_expr(src, env.clone(), arg.clone())?;
                    let arg_env = Env::new_with_parent(lam_env.clone());
                    if !destructure_pattern(src, arg_env.clone(), &param, &varg).0 {
                        return Err(RuntimeError::PatternMismatch(
                            format!("apply {:?}", param).into(),
                            param.span,
                        ));
                    }
                    expr = lam_expr;
                    env = arg_env;
                    continue 'tco;
                }
                Value::NativeFn(fun) => fun.call(eval_expr(src, env.clone(), arg.clone())?)?,
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
            ExprKind::Let(pat, rec, let_expr, body) => {
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
            // ExprKind::Fn(ident, param, fn_expr, body) => {
            //     let lam_env = Env::new_with_parent(env.clone());
            //     let value = Value::Lambda(lam_env.clone(), param.clone(), fn_expr.clone());
            //     env.borrow_mut().insert(ident.name, value);
            //     expr = body.clone();
            //     env = lam_env;
            //     continue 'tco;
            // }
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
) -> (bool, Vec<(InternedString, Value, Type)>) {
    let mut bindings = vec![];

    match pat.kind.as_ref() {
        PatternKind::Wildcard => (true, vec![]),
        PatternKind::Lit(lit) => (
            match val {
                Value::Lit(l) => lit == l,
                _ => false,
            },
            vec![],
        ),
        PatternKind::Ident(ident) => {
            env.borrow_mut().insert(ident.name, val.clone());
            log::debug!("inserted {:?} -> {:?}", ident, val);
            bindings.push((ident.name, val.clone(), pat.ty.clone()));
            (true, bindings)
        }
        PatternKind::List(list) => match val {
            Value::List(vals) => {
                if list.len() != vals.len() {
                    return (false, bindings.clone());
                }
                for (pat, val) in list.iter().zip(vals.iter()) {
                    let (res, new_bindings) = destructure_pattern(src, env.clone(), pat, val);
                    if !res {
                        return (false, bindings.clone());
                    }
                    bindings.extend(new_bindings.clone());
                }
                (true, bindings.clone())
            }
            _ => (false, bindings.clone()),
        },
        PatternKind::Pair(head, tail) => match val {
            Value::List(vals) => {
                if let Some(val_head) = vals.head() {
                    let (res_head, new_bindings_head) =
                        destructure_pattern(src, env.clone(), head, val_head);
                    if !res_head {
                        return (false, bindings.clone());
                    }
                    bindings.extend(new_bindings_head.clone());
                }
                if let Some(val_tail) = vals.tail() {
                    let (res_tail, new_bindings_tail) =
                        destructure_pattern(src, env.clone(), tail, &Value::List(val_tail.clone()));
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
