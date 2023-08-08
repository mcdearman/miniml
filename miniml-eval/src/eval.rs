// use crate::{
//     env::Env,
//     runtime_error::{EvalResult, RuntimeError},
//     value::Value,
// };
// use miniml_syntax::ast::{Decl, Expr, InfixOp, Lit, PrefixOp};
// use std::{cell::RefCell, rc::Rc};

// pub fn eval(env: Rc<RefCell<Env>>, decl: &Decl) -> EvalResult<Value> {
//     match decl.clone() {
//         Decl::Const { name, expr } => {
//             env.borrow_mut()
//                 .define(name.value, eval_expr(env.clone(), &expr.value)?);
//             Ok(Value::Unit)
//         }
//         Decl::Let { name, expr } => {
//             env.borrow_mut()
//                 .define(name.value, eval_expr(env.clone(), &expr.value)?);
//             Ok(Value::Unit)
//         }
//         Decl::Fn { name, params, body } => {
//             env.borrow_mut().define(
//                 name.value,
//                 Value::Lambda {
//                     env: Env::with_parent(env.clone()),
//                     params: params.into_iter().map(|p| p.value).collect(),
//                     body: Box::new(body.value),
//                 },
//             );
//             Ok(Value::Unit)
//         }
//     }
// }

// pub fn eval_expr(env: Rc<RefCell<Env>>, expr: &Expr) -> EvalResult<Value> {
//     match expr.clone() {
//         Expr::Ident(name) => env
//             .borrow()
//             .find(&name)
//             .ok_or(RuntimeError::new(format!("Unbound identifier: {}", name))),
//         Expr::Lit(lit) => match lit {
//             Lit::Int(i) => Ok(Value::Int(i)),
//             Lit::Real(r) => Ok(Value::Real(r)),
//             Lit::String(s) => Ok(Value::String(s)),
//         },
//         Expr::Prefix { op, expr } => match op.value {
//             PrefixOp::Neg => match eval_expr(env.clone(), &expr.value)? {
//                 Value::Int(i) => Ok(Value::Int(-i)),
//                 Value::Real(r) => Ok(Value::Real(-r)),
//                 _ => Err(RuntimeError::from("Invalid operand type")),
//             },
//             PrefixOp::Not => match eval_expr(env.clone(), &expr.value)? {
//                 Value::Bool(b) => Ok(Value::Bool(!b)),
//                 _ => Err(RuntimeError::from("Invalid operand type")),
//             },
//         },
//         Expr::Infix { op, lhs, rhs } => match op.value {
//             InfixOp::Add => match (
//                 eval_expr(env.clone(), &lhs.value)?,
//                 eval_expr(env.clone(), &rhs.value)?,
//             ) {
//                 (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
//                 (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l + r)),
//                 _ => Err(RuntimeError::from("Invalid operand type")),
//             },
//             InfixOp::Sub => match (
//                 eval_expr(env.clone(), &lhs.value)?,
//                 eval_expr(env.clone(), &rhs.value)?,
//             ) {
//                 (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
//                 (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l - r)),
//                 _ => Err(RuntimeError::from("Invalid operand type")),
//             },
//             InfixOp::Mul => match (
//                 eval_expr(env.clone(), &lhs.value)?,
//                 eval_expr(env.clone(), &rhs.value)?,
//             ) {
//                 (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
//                 (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l * r)),
//                 _ => Err(RuntimeError::from("Invalid operand type")),
//             },
//             InfixOp::Div => match (
//                 eval_expr(env.clone(), &lhs.value)?,
//                 eval_expr(env.clone(), &rhs.value)?,
//             ) {
//                 (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l / r)),
//                 (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l / r)),
//                 _ => Err(RuntimeError::from("Invalid operand type")),
//             },
//             InfixOp::Mod => match (
//                 eval_expr(env.clone(), &lhs.value)?,
//                 eval_expr(env.clone(), &rhs.value)?,
//             ) {
//                 (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l % r)),
//                 _ => Err(RuntimeError::from("Invalid operand type")),
//             },
//             InfixOp::Pow => match (
//                 eval_expr(env.clone(), &lhs.value)?,
//                 eval_expr(env.clone(), &rhs.value)?,
//             ) {
//                 (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l.pow(r as u32))),
//                 (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l.powf(r))),
//                 _ => Err(RuntimeError::from("Invalid operand type")),
//             },
//             InfixOp::Eq => {
//                 let l = eval_expr(env.clone(), &lhs.value)?;
//                 let r = eval_expr(env.clone(), &rhs.value)?;
//                 Ok(Value::Bool(l == r))
//             }
//             InfixOp::Neq => {
//                 let l = eval_expr(env.clone(), &lhs.value)?;
//                 let r = eval_expr(env.clone(), &rhs.value)?;
//                 Ok(Value::Bool(l != r))
//             }
//             InfixOp::Lss => todo!(),
//             InfixOp::Gtr => todo!(),
//             InfixOp::Leq => todo!(),
//             InfixOp::Geq => todo!(),
//             InfixOp::And => todo!(),
//             InfixOp::Or => todo!(),
//             InfixOp::Pipe => todo!(),
//         },
//         Expr::Let { name, expr, body } => {
//             let value = eval_expr(env.clone(), &expr.value)?;
//             let let_env = Env::with_parent(env.clone());
//             let_env.borrow_mut().define(name.value, value);
//             eval_expr(let_env.clone(), &body.value)
//         }
//         Expr::Apply { fun, args } => {
//             let fun = eval_expr(env.clone(), &fun.value)?;
//             let vargs = args
//                 .into_iter()
//                 .map(|arg| eval_expr(env.clone(), &arg.value))
//                 .collect::<Result<Vec<_>, _>>()?;
//             match fun {
//                 Value::Lambda {
//                     env: lam_env,
//                     params,
//                     body,
//                 } => {
//                     for (param, arg) in params.into_iter().zip(vargs.into_iter()) {
//                         lam_env.borrow_mut().define(param, arg);
//                     }
//                     eval_expr(lam_env.clone(), &body)
//                 }
//                 _ => Err(RuntimeError::from("Cannot call non-function value")),
//             }
//         }
//         Expr::If { cond, then, else_ } => match eval_expr(env.clone(), &cond.value)? {
//             Value::Bool(true) => eval_expr(env.clone(), &then.value),
//             Value::Bool(false) => eval_expr(env.clone(), &else_.value),
//             _ => Err(RuntimeError::from("Invalid operand type")),
//         },
//         Expr::Lambda { params, body } => Ok(Value::Lambda {
//             env: Env::with_parent(env.clone()),
//             params: params.into_iter().map(|p| p.value).collect(),
//             body: Box::new(body.value),
//         }),
//         Expr::Unit => Ok(Value::Unit),
//         Expr::Error => Err(RuntimeError::from("Error expression")),
//     }
// }
