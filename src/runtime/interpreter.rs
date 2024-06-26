use super::{
    env::Env,
    error::{RuntimeError, RuntimeResult},
    eval::{eval, RuntimePayload},
    value::{Lit, NativeFn, Value},
};
use crate::{
    analysis::infer::{r#type::Type, TypeSolver},
    lex::token_iter::TokenIter,
    parse::parse,
    rename::resolver::Resolver,
    utils::{intern::InternedString, list::List, span::Span, unique_id::UniqueId},
};
use itertools::Itertools;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug)]
pub struct Interpreter {
    res: Resolver,
    type_solver: TypeSolver,
    env: Rc<RefCell<Env>>,
}

impl Interpreter {
    pub fn new() -> Self {
        let res = Resolver::new();
        // let scoped_interner = res.env().dump_to_interner();
        let type_solver = TypeSolver::new();
        let env = Env::new();
        // env.borrow_mut().insert(
        //     "println".into(),
        //     Value::NativeFn(|dict| match dict {
        //         NativeFnArgs { args, len: 1 } => {
        //             println!("{}", args[0]);
        //             Ok(Value::Unit)
        //         }
        //         _ => Err(RuntimeError::ArityError(1, dict.args.len())),
        //     }),
        // );

        // env.borrow_mut().insert(
        //     "__neg__".into(),
        //     Value::NativeFn(|dict| match dict {
        //         NativeFnArgs { args, len: 1 } => match &args[0] {
        //             Value::Lit(Lit::Int(i)) => Ok(Value::Lit(Lit::Int(-i))),
        //             _ => Err(RuntimeError::TypeError(InternedString::from(format!(
        //                 "Expected number, found {:?}",
        //                 args[0]
        //             )))),
        //         },
        //         _ => Err(RuntimeError::ArityError(1, dict.args.len())),
        //     }),
        // );

        // env.borrow_mut().insert(
        //     "__not__".into(),
        //     Value::NativeFn(|dict| match dict {
        //         NativeFnArgs { args, len: 1 } => match &args[0] {
        //             Value::Lit(Lit::Bool(b)) => Ok(Value::Lit(Lit::Bool(!b))),
        //             _ => Err(RuntimeError::TypeError(InternedString::from(format!(
        //                 "Expected bool, found {:?}",
        //                 args[0]
        //             )))),
        //         },
        //         _ => Err(RuntimeError::ArityError(1, dict.args.len())),
        //     }),
        // );

        env.borrow_mut().insert(
            "__add__".into(),
            Value::NativeFn(NativeFn {
                args: vec![],
                len: 2,
                f: |args| {
                    if args.len() != 2 {
                        Err(RuntimeError::ArityError(2, args.len()))
                    } else {
                        match (args[0].clone(), args[1].clone()) {
                            (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                                Ok(Value::Lit(Lit::Int(l + r)))
                            }
                            (Value::Lit(Lit::Rational(l)), Value::Lit(Lit::Rational(r))) => {
                                Ok(Value::Lit(Lit::Rational(l + r)))
                            }
                            _ => Err(RuntimeError::TypeError(InternedString::from(format!(
                                "Expected number, found {:?}",
                                args
                            )))),
                        }
                    }
                },
            }),
        );

        // env.borrow_mut().insert(
        //     "sub".into(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             return Err(RuntimeError::ArityError(2, args.len()));
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
        //                     Ok(Value::Lit(Lit::Int(l - r)))
        //                 }
        //                 (Value::Lit(Lit::Rational(l)), Value::Lit(Lit::Rational(r))) => {
        //                     Ok(Value::Lit(Lit::Rational(l - r)))
        //                 }
        //                 _ => {
        //                     return Err(RuntimeError::TypeError(InternedString::from(format!(
        //                         "Expected number, found {:?}",
        //                         args
        //                     ))));
        //                 }
        //             }
        //         }
        //     }),
        // );

        // env.borrow_mut().insert(
        //     "mul".into(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             return Err(RuntimeError::ArityError(2, args.len()));
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
        //                     Ok(Value::Lit(Lit::Int(l * r)))
        //                 }
        //                 (Value::Lit(Lit::Rational(l)), Value::Lit(Lit::Rational(r))) => {
        //                     Ok(Value::Lit(Lit::Rational(l * r)))
        //                 }
        //                 _ => {
        //                     return Err(RuntimeError::TypeError(InternedString::from(format!(
        //                         "Expected number, found {:?}",
        //                         args
        //                     ))));
        //                 }
        //             }
        //         }
        //     }),
        // );

        // env.borrow_mut().insert(
        //     "div".into(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             return Err(RuntimeError::ArityError(2, args.len()));
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
        //                     if r == &0 {
        //                         return Err(RuntimeError::DivisionByZero);
        //                     }
        //                     Ok(Value::Lit(Lit::Int(l / r)))
        //                 }
        //                 (Value::Lit(Lit::Rational(l)), Value::Lit(Lit::Rational(r))) => {
        //                     Ok(Value::Lit(Lit::Rational(l / r)))
        //                 }
        //                 _ => {
        //                     return Err(RuntimeError::TypeError(InternedString::from(format!(
        //                         "Expected number, found {:?}",
        //                         args
        //                     ))));
        //                 }
        //             }
        //         }
        //     }),
        // );

        // env.borrow_mut().insert(
        //     "rem".into(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             return Err(RuntimeError::ArityError(2, args.len()));
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
        //                     Ok(Value::Lit(Lit::Int(l % r)))
        //                 }
        //                 (Value::Lit(Lit::Rational(l)), Value::Lit(Lit::Rational(r))) => {
        //                     Ok(Value::Lit(Lit::Rational(l % r)))
        //                 }
        //                 _ => {
        //                     return Err(RuntimeError::TypeError(InternedString::from(format!(
        //                         "Expected number, found {:?}",
        //                         args
        //                     ))));
        //                 }
        //             }
        //         }
        //     }),
        // );

        // env.borrow_mut().insert(
        //     "pow".into(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             return Err(RuntimeError::ArityError(2, args.len()));
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
        //                     Ok(Value::Lit(Lit::Int(
        //                         l.checked_pow(
        //                             u32::try_from(*r).map_err(|_| RuntimeError::Overflow)?,
        //                         )
        //                         .unwrap(),
        //                     )))
        //                 }
        //                 _ => {
        //                     return Err(RuntimeError::TypeError(InternedString::from(format!(
        //                         "Expected number, found {:?}",
        //                         args
        //                     ))));
        //                 }
        //             }
        //         }
        //     }),
        // );

        // env.borrow_mut().insert(
        //     "eq".into(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             return Err(RuntimeError::ArityError(2, args.len()));
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (v1, v2) => Ok(Value::Lit(Lit::Bool(v1 == v2))),
        //             }
        //         }
        //     }),
        // );

        // env.borrow_mut().insert(
        //     "neq".into(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             return Err(RuntimeError::ArityError(2, args.len()));
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (v1, v2) => Ok(Value::Lit(Lit::Bool(v1 != v2))),
        //             }
        //         }
        //     }),
        // );

        // env.borrow_mut().insert(
        //     "lt".into(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             return Err(RuntimeError::ArityError(2, args.len()));
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
        //                     Ok(Value::Lit(Lit::Bool(l < r)))
        //                 }
        //                 _ => {
        //                     return Err(RuntimeError::TypeError(InternedString::from(format!(
        //                         "Expected number, found {:?}",
        //                         args
        //                     ))));
        //                 }
        //             }
        //         }
        //     }),
        // );

        // env.borrow_mut().insert(
        //     "lte".into(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             return Err(RuntimeError::ArityError(2, args.len()));
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
        //                     Ok(Value::Lit(Lit::Bool(l <= r)))
        //                 }
        //                 _ => {
        //                     return Err(RuntimeError::TypeError(InternedString::from(format!(
        //                         "Expected number, found {:?}",
        //                         args
        //                     ))));
        //                 }
        //             }
        //         }
        //     }),
        // );

        // env.borrow_mut().insert(
        //     "gt".into(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             return Err(RuntimeError::ArityError(2, args.len()));
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
        //                     Ok(Value::Lit(Lit::Bool(l > r)))
        //                 }
        //                 _ => {
        //                     return Err(RuntimeError::TypeError(InternedString::from(format!(
        //                         "Expected number, found {:?}",
        //                         args
        //                     ))));
        //                 }
        //             }
        //         }
        //     }),
        // );

        // env.borrow_mut().insert(
        //     "gte".into(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             return Err(RuntimeError::ArityError(2, args.len()));
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
        //                     Ok(Value::Lit(Lit::Bool(l >= r)))
        //                 }
        //                 _ => {
        //                     return Err(RuntimeError::TypeError(InternedString::from(format!(
        //                         "Expected number, found {:?}",
        //                         args
        //                     ))));
        //                 }
        //             }
        //         }
        //     }),
        // );

        // env.borrow_mut().insert(
        //     "pair".into(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             return Err(RuntimeError::ArityError(2, args.len()));
        //         } else {
        //             match args.get(1).unwrap() {
        //                 Value::List(List::Empty) => Ok(Value::List(List::Pair {
        //                     head: Box::new(args[0].clone()),
        //                     tail: Box::new(List::Empty),
        //                 })),
        //                 Value::List(tail @ List::Pair { .. }) => Ok(Value::List(List::Pair {
        //                     head: Box::new(args[0].clone()),
        //                     tail: Box::new(tail.clone()),
        //                 })),
        //                 _ => Err(RuntimeError::TypeError(InternedString::from(format!(
        //                     "Expected list, found {:?}",
        //                     args
        //                 )))),
        //             }
        //         }
        //     }),
        // );

        Self {
            res,
            type_solver,
            env,
        }
    }

    pub fn run(&mut self, src: &str) -> RuntimeResult<RuntimePayload> {
        let stream = TokenIter::new(&src);
        log::debug!("Tokens: {:?}", stream.clone().collect_vec());

        let ast = match parse(stream, true) {
            (Some(ast), _) => {
                log::debug!("AST: {:#?}", ast);
                // println!("AST: {:#?}", ast);
                ast
            }
            (None, parse_errors) => {
                return Err(RuntimeError::ParseError(
                    parse_errors
                        .iter()
                        .map(|e| InternedString::from(e.to_string()))
                        .collect_vec(),
                ));
            }
        };

        let nir = match self.res.resolve(&ast) {
            (Some(nir), _) => {
                log::debug!("NIR: {:#?}", nir);
                nir
            }
            (None, res_errors) => {
                return Err(RuntimeError::ResError(
                    res_errors
                        .iter()
                        .map(|e| InternedString::from(e.to_string()))
                        .collect_vec(),
                ));
            }
        };

        let tir = match self.type_solver.infer(&src, &nir) {
            (Some(tir), _) => {
                log::debug!("{:#?}", tir);
                tir
            }
            (None, errors) => {
                return Err(RuntimeError::InferenceError(
                    errors
                        .iter()
                        .map(|e| InternedString::from(e.to_string()))
                        .collect_vec(),
                ));
            }
        };

        eval(&src, self.env.clone(), tir)
    }
}
