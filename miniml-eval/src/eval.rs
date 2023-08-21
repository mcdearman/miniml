use crate::{
    env::Env,
    runtime_error::{EvalResult, RuntimeError},
    value::Value,
};
use miniml_syntax::ast::{Decl, Expr, InfixOp, Lit, PrefixOp, Root};
use miniml_util::{
    intern::InternedString,
    span::{Span, Spannable},
};
use std::{cell::RefCell, rc::Rc};

pub fn eval(env: Rc<RefCell<Env>>, root: &Root) -> EvalResult<Value> {
    for decl in root.clone().decls {
        eval_decl(env.clone(), &decl.value)?;
    }
    if env.borrow().find(&"main".into()).is_none() {
        Ok(Value::Unit)
    } else {
        eval_expr(
            env.clone(),
            &Expr::Apply {
                fun: Box::new(
                    Expr::Ident(InternedString::from("main").spanned(Span::new(0, 0)))
                        .spanned(Span::new(0, 0)),
                ),
                args: vec![],
            },
        )
    }
}

pub fn eval_decl(env: Rc<RefCell<Env>>, decl: &Decl) -> EvalResult<Value> {
    match decl.clone() {
        Decl::Const { name, expr } => {
            let value = eval_expr(env.clone(), &expr.value)?;
            env.borrow_mut().define(name.value, value);
            Ok(Value::Unit)
        }
        Decl::Let { name, expr } => {
            let value = eval_expr(env.clone(), &expr.value)?;
            env.borrow_mut().define(name.value, value);
            Ok(Value::Unit)
        }
        Decl::Fn { name, params, body } => {
            env.borrow_mut().define(
                name.value,
                Value::Lambda {
                    env: env.clone(),
                    params: params.into_iter().map(|p| p.value).collect(),
                    body: Box::new(body.value),
                },
            );
            if &*name.value != "main" {
                env.borrow_mut().define(
                    InternedString::from("main"),
                    Value::Lambda {
                        env: env.clone(),
                        params: vec![],
                        body: Box::new(Expr::Unit),
                    },
                );
            }
            Ok(Value::Unit)
        }
    }
}

pub fn eval_expr(env: Rc<RefCell<Env>>, expr: &Expr) -> EvalResult<Value> {
    match expr.clone() {
        Expr::Ident(name) => env
            .borrow()
            .find(&name.value)
            .ok_or(RuntimeError::new(format!("Unbound identifier: {}", name))),
        Expr::Lit(lit) => match lit.value {
            Lit::Int(i) => Ok(Value::Int(i.0)),
            Lit::Rational(r) => Ok(Value::Rational(r)),
            Lit::Real(r) => Ok(Value::Real(r)),
            Lit::Complex(c) => Ok(Value::Complex(c)),
            Lit::Char(c) => Ok(Value::Char(c)),
            Lit::String(s) => Ok(Value::String(s)),
        },
        Expr::Prefix { op, expr } => match op.value {
            PrefixOp::Neg => match eval_expr(env.clone(), &expr.value)? {
                Value::Int(i) => Ok(Value::Int(-i)),
                Value::Real(r) => Ok(Value::Real(-r)),
                _ => Err(RuntimeError::from("Invalid operand type")),
            },
            PrefixOp::Not => match eval_expr(env.clone(), &expr.value)? {
                Value::Bool(b) => Ok(Value::Bool(!b)),
                _ => Err(RuntimeError::from("Invalid operand type")),
            },
        },
        Expr::Infix { op, lhs, rhs } => match op.value {
            InfixOp::Add => match (
                eval_expr(env.clone(), &lhs.value)?,
                eval_expr(env.clone(), &rhs.value)?,
            ) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
                (Value::Rational(l), Value::Rational(r)) => Ok(Value::Rational(l + r)),
                (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l + r)),
                _ => Err(RuntimeError::from("Invalid operand type")),
            },
            InfixOp::Sub => match (
                eval_expr(env.clone(), &lhs.value)?,
                eval_expr(env.clone(), &rhs.value)?,
            ) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
                (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l - r)),
                _ => Err(RuntimeError::from("Invalid operand type")),
            },
            InfixOp::Mul => match (
                eval_expr(env.clone(), &lhs.value)?,
                eval_expr(env.clone(), &rhs.value)?,
            ) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
                (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l * r)),
                _ => Err(RuntimeError::from("Invalid operand type")),
            },
            InfixOp::Div => match (
                eval_expr(env.clone(), &lhs.value)?,
                eval_expr(env.clone(), &rhs.value)?,
            ) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l / r)),
                (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l / r)),
                _ => Err(RuntimeError::from("Invalid operand type")),
            },
            InfixOp::Rem => match (
                eval_expr(env.clone(), &lhs.value)?,
                eval_expr(env.clone(), &rhs.value)?,
            ) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l % r)),
                _ => Err(RuntimeError::from("Invalid operand type")),
            },
            InfixOp::Pow => match (
                eval_expr(env.clone(), &lhs.value)?,
                eval_expr(env.clone(), &rhs.value)?,
            ) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l.pow(r as u32))),
                (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l.powf(r))),
                _ => Err(RuntimeError::from("Invalid operand type")),
            },
            InfixOp::Eq => {
                let l = eval_expr(env.clone(), &lhs.value)?;
                let r = eval_expr(env.clone(), &rhs.value)?;
                Ok(Value::Bool(l == r))
            }
            InfixOp::Neq => {
                let l = eval_expr(env.clone(), &lhs.value)?;
                let r = eval_expr(env.clone(), &rhs.value)?;
                Ok(Value::Bool(l != r))
            }
            InfixOp::Lt => {
                let l = eval_expr(env.clone(), &lhs.value)?;
                let r = eval_expr(env.clone(), &rhs.value)?;
                Ok(Value::Bool(l < r))
            }
            InfixOp::Gt => {
                let l = eval_expr(env.clone(), &lhs.value)?;
                let r = eval_expr(env.clone(), &rhs.value)?;
                Ok(Value::Bool(l > r))
            }
            InfixOp::Leq => {
                let l = eval_expr(env.clone(), &lhs.value)?;
                let r = eval_expr(env.clone(), &rhs.value)?;
                Ok(Value::Bool(l <= r))
            }
            InfixOp::Geq => {
                let l = eval_expr(env.clone(), &lhs.value)?;
                let r = eval_expr(env.clone(), &rhs.value)?;
                Ok(Value::Bool(l >= r))
            }
            InfixOp::And => {
                let l = eval_expr(env.clone(), &lhs.value)?;
                let r = eval_expr(env.clone(), &rhs.value)?;
                match (l, r) {
                    (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l && r)),
                    _ => Err(RuntimeError::from("Cannot and non-bool values")),
                }
            }
            InfixOp::Or => {
                let l = eval_expr(env.clone(), &lhs.value)?;
                let r = eval_expr(env.clone(), &rhs.value)?;
                match (l, r) {
                    (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l || r)),
                    _ => Err(RuntimeError::from("Cannot or non-bool values")),
                }
            }
            InfixOp::Pipe => todo!(),
            InfixOp::Stmt => {
                eval_expr(env.clone(), &lhs.value)?;
                eval_expr(env.clone(), &rhs.value)
            }
        },
        Expr::Let { name, expr, body } => {
            let value = eval_expr(env.clone(), &expr.value)?;
            let let_env = Env::with_parent(env.clone());
            let_env.borrow_mut().define(name.value, value);
            eval_expr(let_env.clone(), &body.value)
        }
        Expr::Apply { fun, args } => {
            let funv = eval_expr(env.clone(), &fun.value)?;
            let vargs = args
                .clone()
                .into_iter()
                .map(|arg| eval_expr(env.clone(), &arg.value))
                .collect::<Result<Vec<_>, _>>()?;
            // println!("call: {} {:?}", fun.value, vargs);

            match funv.clone() {
                Value::Lambda {
                    env: lam_env,
                    params,
                    body,
                } => {
                    let arg_env = Env::with_parent(lam_env.clone());
                    for (param, arg) in params.into_iter().zip(vargs.into_iter()) {
                        arg_env.borrow_mut().define(param, arg);
                    }
                    let a = eval_expr(arg_env.clone(), &body)?;
                    Ok(a)
                }
                _ => Err(RuntimeError::from("Cannot call non-function value")),
            }
        }
        Expr::If {
            cond,
            then,
            elifs,
            else_,
        } => match eval_expr(env.clone(), &cond.value)? {
            Value::Bool(true) => eval_expr(env.clone(), &then.value),
            Value::Bool(false) => {
                for elif in elifs {
                    if let Value::Bool(true) = eval_expr(env.clone(), &elif.0.value)? {
                        return eval_expr(env.clone(), &elif.1.value);
                    }
                }
                eval_expr(env.clone(), &else_.value)
            }
            _ => Err(RuntimeError::from("Invalid operand type")),
        },
        Expr::Lambda { params, body } => Ok(Value::Lambda {
            env: env.clone(),
            params: params.into_iter().map(|p| p.value).collect(),
            body: Box::new(body.value),
        }),
        Expr::Unit => Ok(Value::Unit),
        Expr::Error => Err(RuntimeError::from("Error expression")),
    }
}
