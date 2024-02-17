use self::{
    env::Env,
    error::RuntimeError,
    value::{Lit, Value},
};
use crate::utils::{intern::InternedString, unique_id::UniqueId};
use std::collections::HashMap;

pub mod env;
pub mod error;
pub mod eval;
pub mod value;

pub fn default_env(builtins: HashMap<UniqueId, InternedString>) -> Env {
    let mut env = Env::new();
    for (id, name) in builtins {
        match name.as_ref() {
            "neg" => {
                env.def(
                    id,
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
            }
            "not" => {
                env.def(
                    id,
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
            }
            "add" => {
                env.def(
                    id,
                    Value::NativeFn(|args| {
                        if args.len() != 2 {
                            return Err(RuntimeError::ArityError(2, args.len()));
                        } else {
                            match (args.get(0).unwrap(), args.get(1).unwrap()) {
                                (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                                    Ok(Value::Lit(Lit::Int(l + r)))
                                }
                                _ => {
                                    return Err(RuntimeError::TypeError(InternedString::from(
                                        format!("Expected numbers got {:?}", args),
                                    )));
                                }
                            }
                        }
                    }),
                );
            }
            "sub" => {
                env.def(
                    id,
                    Value::NativeFn(|args| {
                        if args.len() != 2 {
                            return Err(RuntimeError::ArityError(2, args.len()));
                        } else {
                            match (args.get(0).unwrap(), args.get(1).unwrap()) {
                                (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                                    Ok(Value::Lit(Lit::Int(l - r)))
                                }
                                _ => {
                                    return Err(RuntimeError::TypeError(InternedString::from(
                                        format!("Expected number, found {:?}", args),
                                    )));
                                }
                            }
                        }
                    }),
                );
            }
            "mul" => {
                env.def(
                    id,
                    Value::NativeFn(|args| {
                        if args.len() != 2 {
                            return Err(RuntimeError::ArityError(2, args.len()));
                        } else {
                            match (args.get(0).unwrap(), args.get(1).unwrap()) {
                                (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                                    Ok(Value::Lit(Lit::Int(l * r)))
                                }
                                _ => {
                                    return Err(RuntimeError::TypeError(InternedString::from(
                                        format!("Expected number, found {:?}", args),
                                    )));
                                }
                            }
                        }
                    }),
                );
            }
            "div" => {
                env.def(
                    id,
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
                                _ => {
                                    return Err(RuntimeError::TypeError(InternedString::from(
                                        format!("Expected number, found {:?}", args),
                                    )));
                                }
                            }
                        }
                    }),
                );
            }
            "rem" => {
                env.def(
                    id,
                    Value::NativeFn(|args| {
                        if args.len() != 2 {
                            return Err(RuntimeError::ArityError(2, args.len()));
                        } else {
                            match (args.get(0).unwrap(), args.get(1).unwrap()) {
                                (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                                    Ok(Value::Lit(Lit::Int(l % r)))
                                }
                                _ => {
                                    return Err(RuntimeError::TypeError(InternedString::from(
                                        format!("Expected number, found {:?}", args),
                                    )));
                                }
                            }
                        }
                    }),
                );
            }
            "pow" => {
                env.def(
                    id,
                    Value::NativeFn(|args| {
                        if args.len() != 2 {
                            return Err(RuntimeError::ArityError(2, args.len()));
                        } else {
                            match (args.get(0).unwrap(), args.get(1).unwrap()) {
                                (Value::Lit(Lit::Int(l)), Value::Lit(Lit::Int(r))) => {
                                    Ok(Value::Lit(Lit::Int(
                                        l.checked_pow(
                                            u32::try_from(*r)
                                                .map_err(|_| RuntimeError::Overflow)?,
                                        )
                                        .unwrap(),
                                    )))
                                }
                                _ => {
                                    return Err(RuntimeError::TypeError(InternedString::from(
                                        format!("Expected number, found {:?}", args),
                                    )));
                                }
                            }
                        }
                    }),
                );
            }
            "eq" => {
                env.def(
                    id,
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
            }
            "neq" => {
                env.def(
                    id,
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
            }
            "lt" => {
                env.def(
                    id,
                    Value::NativeFn(|args| {
                        if args.len() != 2 {
                            return Err(RuntimeError::ArityError(2, args.len()));
                        } else {
                            match (args.get(0).unwrap(), args.get(1).unwrap()) {
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
                    }),
                );
            }
            "lte" => {
                env.def(
                    id,
                    Value::NativeFn(|args| {
                        if args.len() != 2 {
                            return Err(RuntimeError::ArityError(2, args.len()));
                        } else {
                            match (args.get(0).unwrap(), args.get(1).unwrap()) {
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
                    }),
                );
            }
            "gt" => {
                env.def(
                    id,
                    Value::NativeFn(|args| {
                        if args.len() != 2 {
                            return Err(RuntimeError::ArityError(2, args.len()));
                        } else {
                            match (args.get(0).unwrap(), args.get(1).unwrap()) {
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
                    }),
                );
            }
            "gte" => {
                env.def(
                    id,
                    Value::NativeFn(|args| {
                        if args.len() != 2 {
                            return Err(RuntimeError::ArityError(2, args.len()));
                        } else {
                            match (args.get(0).unwrap(), args.get(1).unwrap()) {
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
                    }),
                );
            }
            "and" => {
                env.def(
                    id,
                    Value::NativeFn(|args| {
                        if args.len() != 2 {
                            return Err(RuntimeError::ArityError(2, args.len()));
                        } else {
                            match (args.get(0).unwrap(), args.get(1).unwrap()) {
                                (Value::Lit(Lit::Bool(l)), Value::Lit(Lit::Bool(r))) => {
                                    Ok(Value::Lit(Lit::Bool(*l && *r)))
                                }
                                _ => {
                                    return Err(RuntimeError::TypeError(InternedString::from(
                                        format!("Expected bool, found {:?}", args),
                                    )));
                                }
                            }
                        }
                    }),
                );
            }
            "or" => {
                env.def(
                    id,
                    Value::NativeFn(|args| {
                        if args.len() != 2 {
                            return Err(RuntimeError::ArityError(2, args.len()));
                        } else {
                            match (args.get(0).unwrap(), args.get(1).unwrap()) {
                                (Value::Lit(Lit::Bool(l)), Value::Lit(Lit::Bool(r))) => {
                                    Ok(Value::Lit(Lit::Bool(*l || *r)))
                                }
                                _ => {
                                    return Err(RuntimeError::TypeError(InternedString::from(
                                        format!("Expected bool, found {:?}", args),
                                    )));
                                }
                            }
                        }
                    }),
                );
            }
            _ => {}
        }
    }

    env
}
