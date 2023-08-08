// use itertools::join;
// use miniml_syntax::ast::Expr;
// use miniml_util::{intern::InternedString, list::List};
// use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

// use crate::env::Env;

// #[derive(Debug, Clone)]
// pub enum Value {
//     Int(i64),
//     Bool(bool),
//     Real(f64),
//     String(InternedString),
//     List(List<Value>),
//     Tuple(Vec<Value>),
//     Record(HashMap<InternedString, Value>),
//     Lambda {
//         env: Rc<RefCell<Env>>,
//         params: Vec<InternedString>,
//         body: Box<Expr>,
//     },
//     Unit,
// }

// impl Display for Value {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Value::Int(n) => write!(f, "{}", n),
//             Value::Bool(b) => write!(f, "{}", b),
//             Value::Real(r) => write!(f, "{}", r),
//             Value::String(s) => write!(f, "{}", s),
//             Value::List(l) => write!(f, "{}", l),
//             Value::Tuple(t) => write!(f, "({})", join(t.clone(), ", ")),
//             Value::Record(r) => write!(
//                 f,
//                 "{{{}}}",
//                 join(
//                     r.clone().into_iter().map(|(k, v)| format!("{} = {}", k, v)),
//                     ", "
//                 )
//             ),
//             Value::Lambda { env, params, body } => write!(
//                 f,
//                 "fn {} => {}",
//                 join(params.clone().into_iter(), ", "),
//                 body
//             ),
//             Value::Unit => write!(f, "()"),
//         }
//     }
// }

// impl PartialEq for Value {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (Self::Int(l0), Self::Int(r0)) => l0 == r0,
//             (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
//             (Self::Real(l0), Self::Real(r0)) => l0 == r0,
//             (Self::String(l0), Self::String(r0)) => l0 == r0,
//             (Self::List(l0), Self::List(r0)) => l0 == r0,
//             (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
//             (Self::Record(l0), Self::Record(r0)) => l0 == r0,
//             (
//                 Self::Lambda {
//                     env: l_env,
//                     params: l_params,
//                     body: l_body,
//                 },
//                 Self::Lambda {
//                     env: r_env,
//                     params: r_params,
//                     body: r_body,
//                 },
//             ) => l_params == r_params && l_body == r_body,
//             _ => false,
//         }
//     }
// }
