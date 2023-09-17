// use crate::env::Env;
// use itertools::join;
// use miniml_syntax::ast::Expr;
// use miniml_util::{intern::InternedString, list::List};
// use num_complex::Complex;
// use num_rational::Rational64;
// use std::{cell::RefCell, collections::HashMap, fmt::Display, ops::Add, rc::Rc};

// #[derive(Debug, Clone)]
// pub enum Value {
//     Int(i64),
//     Rational(Rational64),
//     Real(f64),
//     Complex(Complex<f64>),
//     Bool(bool),
//     Char(char),
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
//             Self::Rational(r) => write!(f, "{}", r),
//             Value::Real(r) => write!(f, "{}", r),
//             Value::Complex(c) => write!(f, "{}", c),
//             Value::Bool(b) => write!(f, "{}", b),
//             Value::Char(c) => write!(f, "{}", c),
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
//             (Self::Int(l), Self::Int(r)) => l == r,
//             (Self::Bool(l), Self::Bool(r)) => l == r,
//             (Self::Real(l), Self::Real(r)) => l == r,
//             (Self::String(l), Self::String(r)) => l == r,
//             (Self::List(l), Self::List(r)) => l == r,
//             (Self::Tuple(l), Self::Tuple(r)) => l == r,
//             (Self::Record(l), Self::Record(r)) => l == r,
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

// impl PartialOrd for Value {
//     fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
//         match (self, other) {
//             (Self::Int(l), Self::Int(r)) => l.partial_cmp(r),
//             (Self::Real(l), Self::Real(r)) => l.partial_cmp(r),
//             _ => None,
//         }
//     }
// }

