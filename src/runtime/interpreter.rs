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

        env.borrow_mut().insert(
            "__sub__".into(),
            Value::NativeFn(NativeFn {
                args: vec![],
                len: 2,
                f: |args| {
                    if args.len() != 2 {
                        Err(RuntimeError::ArityError(2, args.len()))
                    } else {
                        match (args[0].clone(), args[1].clone()) {
                            (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                                Ok(Value::Lit(Lit::Int(l - r)))
                            }
                            (Value::Lit(Lit::Rational(l)), Value::Lit(Lit::Rational(r))) => {
                                Ok(Value::Lit(Lit::Rational(l - r)))
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

        env.borrow_mut().insert(
            "__mul__".into(),
            Value::NativeFn(NativeFn {
                args: vec![],
                len: 2,
                f: |args| {
                    if args.len() != 2 {
                        Err(RuntimeError::ArityError(2, args.len()))
                    } else {
                        match (args[0].clone(), args[1].clone()) {
                            (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                                Ok(Value::Lit(Lit::Int(l * r)))
                            }
                            (Value::Lit(Lit::Rational(l)), Value::Lit(Lit::Rational(r))) => {
                                Ok(Value::Lit(Lit::Rational(l * r)))
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

        env.borrow_mut().insert(
            "__div__".into(),
            Value::NativeFn(NativeFn {
                args: vec![],
                len: 2,
                f: |args| {
                    if args.len() != 2 {
                        Err(RuntimeError::ArityError(2, args.len()))
                    } else {
                        match (args[0].clone(), args[1].clone()) {
                            (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                                Ok(Value::Lit(Lit::Int(l / r)))
                            }
                            (Value::Lit(Lit::Rational(l)), Value::Lit(Lit::Rational(r))) => {
                                Ok(Value::Lit(Lit::Rational(l / r)))
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

        env.borrow_mut().insert(
            "__rem__".into(),
            Value::NativeFn(NativeFn {
                args: vec![],
                len: 2,
                f: |args| {
                    if args.len() != 2 {
                        Err(RuntimeError::ArityError(2, args.len()))
                    } else {
                        match (args[0].clone(), args[1].clone()) {
                            (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                                Ok(Value::Lit(Lit::Int(l % r)))
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

        env.borrow_mut().insert(
            "__pow__".into(),
            Value::NativeFn(NativeFn {
                args: vec![],
                len: 2,
                f: |args| {
                    if args.len() != 2 {
                        Err(RuntimeError::ArityError(2, args.len()))
                    } else {
                        match (args[0].clone(), args[1].clone()) {
                            (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                                Ok(Value::Lit(Lit::Int(l.pow(r as u32))))
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

        env.borrow_mut().insert(
            "__eq__".into(),
            Value::NativeFn(NativeFn {
                args: vec![],
                len: 2,
                f: |args| {
                    if args.len() != 2 {
                        Err(RuntimeError::ArityError(2, args.len()))
                    } else {
                        match (args[0].clone(), args[1].clone()) {
                            (v1, v2) => Ok(Value::Lit(Lit::Bool(v1 == v2))),
                        }
                    }
                },
            }),
        );

        env.borrow_mut().insert(
            "__neq__".into(),
            Value::NativeFn(NativeFn {
                args: vec![],
                len: 2,
                f: |args| {
                    if args.len() != 2 {
                        Err(RuntimeError::ArityError(2, args.len()))
                    } else {
                        match (args[0].clone(), args[1].clone()) {
                            (v1, v2) => Ok(Value::Lit(Lit::Bool(v1 != v2))),
                        }
                    }
                },
            }),
        );

        env.borrow_mut().insert(
            "__lt__".into(),
            Value::NativeFn(NativeFn {
                args: vec![],
                len: 2,
                f: |args| {
                    if args.len() != 2 {
                        Err(RuntimeError::ArityError(2, args.len()))
                    } else {
                        match (args[0].clone(), args[1].clone()) {
                            (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                                Ok(Value::Lit(Lit::Bool(l < r)))
                            }
                            _ => {
                                return Err(RuntimeError::TypeError(InternedString::from(
                                    format!("Expected number, found {:?}", args),
                                )));
                            }
                        }
                    }
                },
            }),
        );

        env.borrow_mut().insert(
            "__lte__".into(),
            Value::NativeFn(NativeFn {
                args: vec![],
                len: 2,
                f: |args| {
                    if args.len() != 2 {
                        Err(RuntimeError::ArityError(2, args.len()))
                    } else {
                        match (args[0].clone(), args[1].clone()) {
                            (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                                Ok(Value::Lit(Lit::Bool(l <= r)))
                            }
                            _ => {
                                return Err(RuntimeError::TypeError(InternedString::from(
                                    format!("Expected number, found {:?}", args),
                                )));
                            }
                        }
                    }
                },
            }),
        );

        env.borrow_mut().insert(
            "__gt__".into(),
            Value::NativeFn(NativeFn {
                args: vec![],
                len: 2,
                f: |args| {
                    if args.len() != 2 {
                        Err(RuntimeError::ArityError(2, args.len()))
                    } else {
                        match (args[0].clone(), args[1].clone()) {
                            (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                                Ok(Value::Lit(Lit::Bool(l > r)))
                            }
                            _ => {
                                return Err(RuntimeError::TypeError(InternedString::from(
                                    format!("Expected number, found {:?}", args),
                                )));
                            }
                        }
                    }
                },
            }),
        );

        env.borrow_mut().insert(
            "__gte__".into(),
            Value::NativeFn(NativeFn {
                args: vec![],
                len: 2,
                f: |args| {
                    if args.len() != 2 {
                        Err(RuntimeError::ArityError(2, args.len()))
                    } else {
                        match (args[0].clone(), args[1].clone()) {
                            (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                                Ok(Value::Lit(Lit::Bool(l >= r)))
                            }
                            _ => {
                                return Err(RuntimeError::TypeError(InternedString::from(
                                    format!("Expected number, found {:?}", args),
                                )));
                            }
                        }
                    }
                },
            }),
        );

        env.borrow_mut().insert(
            "__pair__".into(),
            Value::NativeFn(NativeFn {
                args: vec![],
                len: 2,
                f: |args| {
                    if args.len() != 2 {
                        Err(RuntimeError::ArityError(2, args.len()))
                    } else {
                        match args[1].clone() {
                            Value::List(List::Empty) => Ok(Value::List(List::Pair {
                                head: Box::new(args[0].clone()),
                                tail: Box::new(List::Empty),
                            })),
                            Value::List(tail @ List::Pair { .. }) => Ok(Value::List(List::Pair {
                                head: Box::new(args[0].clone()),
                                tail: Box::new(tail.clone()),
                            })),
                            _ => Err(RuntimeError::TypeError(InternedString::from(format!(
                                "Expected list, found {:?}",
                                args
                            )))),
                        }
                    }
                },
            }),
        );

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
