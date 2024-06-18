use super::{
    env::Env,
    error::{RuntimeError, RuntimeResult},
    eval::{eval, RuntimePayload},
    value::{Lit, Value},
};
use crate::{
    analysis::infer::{r#type::Type, TypeSolver},
    lex::token_iter::TokenIter,
    parse::parse,
    rename::resolver::Resolver,
    utils::{intern::InternedString, list::List, unique_id::UniqueId},
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
        env.borrow_mut().insert(
            "println".into(),
            Value::NativeFn(|args| {
                if let Some(arg) = args.get(0) {
                    println!("{}", arg);
                } else {
                    return Err(RuntimeError::ArityError(1, args.len()));
                }
                Ok(Value::Unit)
            }),
        );

        env.borrow_mut().insert(
            "neg".into(),
            Value::NativeFn(|args| {
                if args.len() != 1 {
                    Err(RuntimeError::ArityError(1, args.len()))
                } else {
                    match args.get(0).unwrap() {
                        Value::Lit(Lit::Int(i)) => Ok(Value::Lit(Lit::Int(-i))),
                        _ => Err(RuntimeError::TypeError(InternedString::from(format!(
                            "Expected number, found {:?}",
                            args
                        )))),
                    }
                }
            }),
        );

        env.borrow_mut().insert(
            "not".into(),
            Value::NativeFn(|args| {
                if args.len() != 1 {
                    return Err(RuntimeError::ArityError(1, args.len()));
                } else {
                    match args.get(0).unwrap() {
                        Value::Lit(Lit::Bool(b)) => Ok(Value::Lit(Lit::Bool(!b))),
                        _ => Err(RuntimeError::TypeError(InternedString::from(format!(
                            "Expected bool, found {:?}",
                            args
                        )))),
                    }
                }
            }),
        );

        env.borrow_mut().insert(
            "add".into(),
            Value::NativeFn(|args| {
                if args.len() != 2 {
                    return Err(RuntimeError::ArityError(2, args.len()));
                } else {
                    match (args.get(0).unwrap(), args.get(1).unwrap()) {
                        (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                            Ok(Value::Lit(Lit::Int(l + r)))
                        }
                        (Value::Lit(Lit::Rational(l)), Value::Lit(Lit::Rational(r))) => {
                            Ok(Value::Lit(Lit::Rational(l + r)))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(InternedString::from(format!(
                                "Expected numbers got {:?}",
                                args
                            ))));
                        }
                    }
                }
            }),
        );

        env.borrow_mut().insert(
            "sub".into(),
            Value::NativeFn(|args| {
                if args.len() != 2 {
                    return Err(RuntimeError::ArityError(2, args.len()));
                } else {
                    match (args.get(0).unwrap(), args.get(1).unwrap()) {
                        (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                            Ok(Value::Lit(Lit::Int(l - r)))
                        }
                        (Value::Lit(Lit::Rational(l)), Value::Lit(Lit::Rational(r))) => {
                            Ok(Value::Lit(Lit::Rational(l - r)))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(InternedString::from(format!(
                                "Expected number, found {:?}",
                                args
                            ))));
                        }
                    }
                }
            }),
        );

        env.borrow_mut().insert(
            "mul".into(),
            Value::NativeFn(|args| {
                if args.len() != 2 {
                    return Err(RuntimeError::ArityError(2, args.len()));
                } else {
                    match (args.get(0).unwrap(), args.get(1).unwrap()) {
                        (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                            Ok(Value::Lit(Lit::Int(l * r)))
                        }
                        (Value::Lit(Lit::Rational(l)), Value::Lit(Lit::Rational(r))) => {
                            Ok(Value::Lit(Lit::Rational(l * r)))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(InternedString::from(format!(
                                "Expected number, found {:?}",
                                args
                            ))));
                        }
                    }
                }
            }),
        );

        env.borrow_mut().insert(
            "div".into(),
            Value::NativeFn(|args| {
                if args.len() != 2 {
                    return Err(RuntimeError::ArityError(2, args.len()));
                } else {
                    match (args.get(0).unwrap(), args.get(1).unwrap()) {
                        (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                            if r == &0 {
                                return Err(RuntimeError::DivisionByZero);
                            }
                            Ok(Value::Lit(Lit::Int(l / r)))
                        }
                        (Value::Lit(Lit::Rational(l)), Value::Lit(Lit::Rational(r))) => {
                            Ok(Value::Lit(Lit::Rational(l / r)))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(InternedString::from(format!(
                                "Expected number, found {:?}",
                                args
                            ))));
                        }
                    }
                }
            }),
        );

        env.borrow_mut().insert(
            "rem".into(),
            Value::NativeFn(|args| {
                if args.len() != 2 {
                    return Err(RuntimeError::ArityError(2, args.len()));
                } else {
                    match (args.get(0).unwrap(), args.get(1).unwrap()) {
                        (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                            Ok(Value::Lit(Lit::Int(l % r)))
                        }
                        (Value::Lit(Lit::Rational(l)), Value::Lit(Lit::Rational(r))) => {
                            Ok(Value::Lit(Lit::Rational(l % r)))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(InternedString::from(format!(
                                "Expected number, found {:?}",
                                args
                            ))));
                        }
                    }
                }
            }),
        );

        env.borrow_mut().insert(
            "pow".into(),
            Value::NativeFn(|args| {
                if args.len() != 2 {
                    return Err(RuntimeError::ArityError(2, args.len()));
                } else {
                    match (args.get(0).unwrap(), args.get(1).unwrap()) {
                        (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                            Ok(Value::Lit(Lit::Int(
                                l.checked_pow(
                                    u32::try_from(*r).map_err(|_| RuntimeError::Overflow)?,
                                )
                                .unwrap(),
                            )))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(InternedString::from(format!(
                                "Expected number, found {:?}",
                                args
                            ))));
                        }
                    }
                }
            }),
        );

        env.borrow_mut().insert(
            "eq".into(),
            Value::NativeFn(|args| {
                if args.len() != 2 {
                    return Err(RuntimeError::ArityError(2, args.len()));
                } else {
                    match (args.get(0).unwrap(), args.get(1).unwrap()) {
                        (v1, v2) => Ok(Value::Lit(Lit::Bool(v1 == v2))),
                    }
                }
            }),
        );

        env.borrow_mut().insert(
            "neq".into(),
            Value::NativeFn(|args| {
                if args.len() != 2 {
                    return Err(RuntimeError::ArityError(2, args.len()));
                } else {
                    match (args.get(0).unwrap(), args.get(1).unwrap()) {
                        (v1, v2) => Ok(Value::Lit(Lit::Bool(v1 != v2))),
                    }
                }
            }),
        );

        env.borrow_mut().insert(
            "lt".into(),
            Value::NativeFn(|args| {
                if args.len() != 2 {
                    return Err(RuntimeError::ArityError(2, args.len()));
                } else {
                    match (args.get(0).unwrap(), args.get(1).unwrap()) {
                        (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                            Ok(Value::Lit(Lit::Bool(l < r)))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(InternedString::from(format!(
                                "Expected number, found {:?}",
                                args
                            ))));
                        }
                    }
                }
            }),
        );

        env.borrow_mut().insert(
            "lte".into(),
            Value::NativeFn(|args| {
                if args.len() != 2 {
                    return Err(RuntimeError::ArityError(2, args.len()));
                } else {
                    match (args.get(0).unwrap(), args.get(1).unwrap()) {
                        (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                            Ok(Value::Lit(Lit::Bool(l <= r)))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(InternedString::from(format!(
                                "Expected number, found {:?}",
                                args
                            ))));
                        }
                    }
                }
            }),
        );

        env.borrow_mut().insert(
            "gt".into(),
            Value::NativeFn(|args| {
                if args.len() != 2 {
                    return Err(RuntimeError::ArityError(2, args.len()));
                } else {
                    match (args.get(0).unwrap(), args.get(1).unwrap()) {
                        (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                            Ok(Value::Lit(Lit::Bool(l > r)))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(InternedString::from(format!(
                                "Expected number, found {:?}",
                                args
                            ))));
                        }
                    }
                }
            }),
        );

        env.borrow_mut().insert(
            "gte".into(),
            Value::NativeFn(|args| {
                if args.len() != 2 {
                    return Err(RuntimeError::ArityError(2, args.len()));
                } else {
                    match (args.get(0).unwrap(), args.get(1).unwrap()) {
                        (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                            Ok(Value::Lit(Lit::Bool(l >= r)))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(InternedString::from(format!(
                                "Expected number, found {:?}",
                                args
                            ))));
                        }
                    }
                }
            }),
        );

        env.borrow_mut().insert(
            "pair".into(),
            Value::NativeFn(|args| {
                if args.len() != 2 {
                    return Err(RuntimeError::ArityError(2, args.len()));
                } else {
                    match args.get(1).unwrap() {
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
