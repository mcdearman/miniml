use crate::{
    analysis::{
        infer::{self, Expr, Item, Root, TyVar},
        res,
    },
    util::{intern::InternedString, node::Node, unique_id::UniqueId},
};
use clap::builder::FalseyValueParser;
use itertools::Itertools;
use num_rational::Rational64;
use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

use super::repl;

pub type RuntimeError = InternedString;
pub type RuntimeResult<T> = Result<T, RuntimeError>;

#[derive(Clone, PartialEq)]
pub struct Env {
    parent: Option<Rc<RefCell<Env>>>,
    bindings: HashMap<UniqueId, Value>,
}

impl Env {
    pub fn new() -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: None,
            bindings: HashMap::new(),
        }))
    }

    pub fn new_with_parent(parent: Rc<RefCell<Env>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            parent: Some(parent),
            bindings: HashMap::new(),
        }))
    }

    pub fn insert(&mut self, id: UniqueId, value: Value) {
        self.bindings.insert(id, value);
    }

    pub fn get(&self, id: &UniqueId) -> Option<Value> {
        self.bindings.get(id).cloned().or(self
            .parent
            .as_ref()
            .and_then(|parent| parent.borrow().get(id).clone()))
    }
}

impl Debug for Env {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = f.debug_struct("Env");
        builder.field(
            "parent",
            if let Some(_) = &self.parent {
                &"Some"
            } else {
                &"None"
            },
        );
        builder.field("bindings", &self.bindings);
        builder.finish()
    }
}

pub fn primitive_ids() -> HashMap<InternedString, UniqueId> {
    let mut ids = HashMap::new();
    ids.insert(InternedString::from("+"), UniqueId::gen());
    ids.insert(InternedString::from("-"), UniqueId::gen());
    ids.insert(InternedString::from("*"), UniqueId::gen());
    ids.insert(InternedString::from("/"), UniqueId::gen());
    ids.insert(InternedString::from("%"), UniqueId::gen());
    ids.insert(InternedString::from("=="), UniqueId::gen());
    ids.insert(InternedString::from("!="), UniqueId::gen());
    ids.insert(InternedString::from("<"), UniqueId::gen());
    ids.insert(InternedString::from(">"), UniqueId::gen());
    ids.insert(InternedString::from("<="), UniqueId::gen());
    ids.insert(InternedString::from(">="), UniqueId::gen());
    ids.insert(InternedString::from("!"), UniqueId::gen());
    ids.insert(InternedString::from("&&"), UniqueId::gen());
    ids.insert(InternedString::from("||"), UniqueId::gen());
    ids.insert(InternedString::from("print"), UniqueId::gen());
    ids
}

pub fn default_ctx(ops: HashMap<InternedString, UniqueId>) -> infer::Context {
    let mut ctx = infer::Context::new();
    ctx.extend(
        ops.get(&InternedString::from("+")).unwrap().clone(),
        infer::Scheme::new(
            vec![],
            infer::Type::Lambda(
                vec![infer::Type::Num, infer::Type::Num],
                Box::new(infer::Type::Num),
            ),
        ),
    );
    ctx.extend(
        ops.get(&InternedString::from("-")).unwrap().clone(),
        infer::Scheme::new(
            vec![],
            infer::Type::Lambda(
                vec![infer::Type::Num, infer::Type::Num],
                Box::new(infer::Type::Num),
            ),
        ),
    );
    ctx.extend(
        ops.get(&InternedString::from("*")).unwrap().clone(),
        infer::Scheme::new(
            vec![],
            infer::Type::Lambda(
                vec![infer::Type::Num, infer::Type::Num],
                Box::new(infer::Type::Num),
            ),
        ),
    );
    ctx.extend(
        ops.get(&InternedString::from("/")).unwrap().clone(),
        infer::Scheme::new(
            vec![],
            infer::Type::Lambda(
                vec![infer::Type::Num, infer::Type::Num],
                Box::new(infer::Type::Num),
            ),
        ),
    );
    ctx.extend(
        ops.get(&InternedString::from("%")).unwrap().clone(),
        infer::Scheme::new(
            vec![],
            infer::Type::Lambda(
                vec![infer::Type::Num, infer::Type::Num],
                Box::new(infer::Type::Num),
            ),
        ),
    );
    let var = TyVar::from(0);
    ctx.extend(
        ops.get(&InternedString::from("==")).unwrap().clone(),
        infer::Scheme::new(
            vec![var],
            infer::Type::Lambda(
                vec![infer::Type::Var(var), infer::Type::Var(var)],
                Box::new(infer::Type::Bool),
            ),
        ),
    );
    ctx.extend(
        ops.get(&InternedString::from("!=")).unwrap().clone(),
        infer::Scheme::new(
            vec![var],
            infer::Type::Lambda(
                vec![infer::Type::Var(var), infer::Type::Var(var)],
                Box::new(infer::Type::Bool),
            ),
        ),
    );
    ctx.extend(
        ops.get(&InternedString::from("<")).unwrap().clone(),
        infer::Scheme::new(
            vec![var],
            infer::Type::Lambda(
                vec![infer::Type::Var(var), infer::Type::Var(var)],
                Box::new(infer::Type::Bool),
            ),
        ),
    );
    ctx.extend(
        ops.get(&InternedString::from(">")).unwrap().clone(),
        infer::Scheme::new(
            vec![var],
            infer::Type::Lambda(
                vec![infer::Type::Var(var), infer::Type::Var(var)],
                Box::new(infer::Type::Bool),
            ),
        ),
    );
    ctx.extend(
        ops.get(&InternedString::from("<=")).unwrap().clone(),
        infer::Scheme::new(
            vec![var],
            infer::Type::Lambda(
                vec![infer::Type::Var(var), infer::Type::Var(var)],
                Box::new(infer::Type::Bool),
            ),
        ),
    );
    ctx.extend(
        ops.get(&InternedString::from(">=")).unwrap().clone(),
        infer::Scheme::new(
            vec![var],
            infer::Type::Lambda(
                vec![infer::Type::Var(var), infer::Type::Var(var)],
                Box::new(infer::Type::Bool),
            ),
        ),
    );
    ctx.extend(
        ops.get(&InternedString::from("print")).unwrap().clone(),
        infer::Scheme::new(
            vec![],
            infer::Type::Lambda(
                vec![infer::Type::Var(TyVar::from(0))],
                Box::new(infer::Type::Unit),
            ),
        ),
    );
    ctx
}

pub fn default_res_env(prims: HashMap<InternedString, UniqueId>) -> Rc<RefCell<res::Env>> {
    let env = res::Env::new();
    for (name, id) in prims {
        env.borrow_mut().insert(name, id);
    }
    env
}

pub fn default_env(ops: HashMap<InternedString, UniqueId>) -> Rc<RefCell<Env>> {
    let env = Env::new();
    env.borrow_mut().insert(
        ops.get(&InternedString::from("+")).unwrap().clone(),
        Value::NativeFn(|args| {
            if args.len() != 2 {
                return Err(format!("Expected 2 args, found {}", args.len()).into());
            } else {
                match (args.get(0).unwrap(), args.get(1).unwrap()) {
                    (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
                        Ok(Value::Lit(Lit::Num(l + r)))
                    }
                    _ => {
                        return Err(format!("Expected number, found {:?}", args).into());
                    }
                }
            }
        }),
    );
    env.borrow_mut().insert(
        ops.get(&InternedString::from("-")).unwrap().clone(),
        Value::NativeFn(|args| {
            if args.len() != 2 {
                return Err(format!("Expected 2 args, found {}", args.len()).into());
            } else {
                match (args.get(0).unwrap(), args.get(1).unwrap()) {
                    (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
                        Ok(Value::Lit(Lit::Num(l - r)))
                    }
                    _ => {
                        return Err(format!("Expected number, found {:?}", args).into());
                    }
                }
            }
        }),
    );
    env.borrow_mut().insert(
        ops.get(&InternedString::from("*")).unwrap().clone(),
        Value::NativeFn(|args| {
            if args.len() != 2 {
                return Err(format!("Expected 2 args, found {}", args.len()).into());
            } else {
                match (args.get(0).unwrap(), args.get(1).unwrap()) {
                    (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
                        Ok(Value::Lit(Lit::Num(l * r)))
                    }
                    _ => {
                        return Err(format!("Expected number, found {:?}", args).into());
                    }
                }
            }
        }),
    );
    env.borrow_mut().insert(
        ops.get(&InternedString::from("/")).unwrap().clone(),
        Value::NativeFn(|args| {
            if args.len() != 2 {
                return Err(format!("Expected 2 args, found {}", args.len()).into());
            } else {
                match (args.get(0).unwrap(), args.get(1).unwrap()) {
                    (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
                        if r == &Rational64::from_integer(0) {
                            return Err(format!("Division by zero").into());
                        }
                        Ok(Value::Lit(Lit::Num(l / r)))
                    }
                    _ => {
                        return Err(format!("Expected number, found {:?}", args).into());
                    }
                }
            }
        }),
    );
    env.borrow_mut().insert(
        ops.get(&InternedString::from("%")).unwrap().clone(),
        Value::NativeFn(|args| {
            if args.len() != 2 {
                return Err(format!("Expected 2 args, found {}", args.len()).into());
            } else {
                match (args.get(0).unwrap(), args.get(1).unwrap()) {
                    (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
                        Ok(Value::Lit(Lit::Num(l % r)))
                    }
                    _ => {
                        return Err(format!("Expected number, found {:?}", args).into());
                    }
                }
            }
        }),
    );
    env.borrow_mut().insert(
        ops.get(&InternedString::from("==")).unwrap().clone(),
        Value::NativeFn(|args| {
            if args.len() != 2 {
                Err(format!("Expected 2 args, found {}", args.len()).into())
            } else {
                match (args.get(0).unwrap(), args.get(1).unwrap()) {
                    (Value::Lit(l), Value::Lit(r)) => Ok(Value::Lit(Lit::Bool(l == r))),
                    (
                        Value::Lambda {
                            env: _,
                            params: p1,
                            body: b1,
                        },
                        Value::Lambda {
                            env: _,
                            params: p2,
                            body: b2,
                        },
                    ) => {
                        if p1.len() != p2.len() {
                            return Ok(Value::Lit(Lit::Bool(false)));
                        } else {
                            let mut p = true;
                            for (p1, p2) in p1.iter().zip(p2) {
                                p = *p1.inner() != *p2.inner();
                                break;
                            }
                            Ok(Value::Lit(Lit::Bool(b1 == b2 && p)))
                        }
                    }
                    (Value::Unit, Value::Unit) => Ok(Value::Lit(Lit::Bool(true))),
                    _ => Ok(Value::Lit(Lit::Bool(false))),
                }
            }
        }),
    );
    env.borrow_mut().insert(
        ops.get(&InternedString::from("!=")).unwrap().clone(),
        Value::NativeFn(|args| {
            if args.len() != 2 {
                Err(format!("Expected 2 args, found {}", args.len()).into())
            } else {
                match (args.get(0).unwrap(), args.get(1).unwrap()) {
                    (Value::Lit(l), Value::Lit(r)) => Ok(Value::Lit(Lit::Bool(l != r))),
                    (
                        Value::Lambda {
                            env: _,
                            params: p1,
                            body: b1,
                        },
                        Value::Lambda {
                            env: _,
                            params: p2,
                            body: b2,
                        },
                    ) => {
                        if p1.len() != p2.len() {
                            return Ok(Value::Lit(Lit::Bool(true)));
                        } else {
                            let mut p = true;
                            for (p1, p2) in p1.iter().zip(p2) {
                                p = *p1.inner() != *p2.inner();
                                break;
                            }
                            Ok(Value::Lit(Lit::Bool(b1 != b2 || p)))
                        }
                    }
                    (Value::Unit, Value::Unit) => Ok(Value::Lit(Lit::Bool(false))),
                    _ => Ok(Value::Lit(Lit::Bool(true))),
                }
            }
        }),
    );
    env.borrow_mut().insert(
        ops.get(&InternedString::from("<")).unwrap().clone(),
        Value::NativeFn(|args| {
            if args.len() != 2 {
                Err(format!("Expected 2 args, found {}", args.len()).into())
            } else {
                match (args.get(0).unwrap(), args.get(1).unwrap()) {
                    (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
                        Ok(Value::Lit(Lit::Bool(l < r)))
                    }
                    _ => {
                        return Err(format!("Expected number, found {:?}", args).into());
                    }
                }
            }
        }),
    );
    env.borrow_mut().insert(
        ops.get(&InternedString::from(">")).unwrap().clone(),
        Value::NativeFn(|args| {
            if args.len() != 2 {
                Err(format!("Expected 2 args, found {}", args.len()).into())
            } else {
                match (args.get(0).unwrap(), args.get(1).unwrap()) {
                    (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
                        Ok(Value::Lit(Lit::Bool(l > r)))
                    }
                    _ => {
                        return Err(format!("Expected number, found {:?}", args).into());
                    }
                }
            }
        }),
    );
    env.borrow_mut().insert(
        ops.get(&InternedString::from("<=")).unwrap().clone(),
        Value::NativeFn(|args| {
            if args.len() != 2 {
                Err(format!("Expected 2 args, found {}", args.len()).into())
            } else {
                match (args.get(0).unwrap(), args.get(1).unwrap()) {
                    (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
                        Ok(Value::Lit(Lit::Bool(l <= r)))
                    }
                    _ => {
                        return Err(format!("Expected number, found {:?}", args).into());
                    }
                }
            }
        }),
    );
    env.borrow_mut().insert(
        ops.get(&InternedString::from(">=")).unwrap().clone(),
        Value::NativeFn(|args| {
            if args.len() != 2 {
                Err(format!("Expected 2 args, found {}", args.len()).into())
            } else {
                match (args.get(0).unwrap(), args.get(1).unwrap()) {
                    (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
                        Ok(Value::Lit(Lit::Bool(l >= r)))
                    }
                    _ => {
                        return Err(format!("Expected number, found {:?}", args).into());
                    }
                }
            }
        }),
    );
    env.borrow_mut().insert(
        ops.get(&InternedString::from("print")).unwrap().clone(),
        Value::NativeFn(|args| {
            if args.len() != 1 {
                Err(format!("Expected 1 arg, found {}", args.len()).into())
            } else {
                println!("{}", args.get(0).unwrap());
                Ok(Value::Unit)
            }
        }),
    );
    env
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Lit(Lit),
    Lambda {
        env: Rc<RefCell<Env>>,
        params: Vec<Node<UniqueId>>,
        body: Node<Expr>,
    },
    NativeFn(fn(Vec<Value>) -> RuntimeResult<Value>),
    Unit,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Lit(Lit::Num(num)) => write!(f, "{}", num),
            Value::Lit(Lit::Bool(b)) => write!(f, "{}", b),
            Value::Lambda { .. } => write!(f, "<lambda>"),
            Value::NativeFn { .. } => write!(f, "<native fn>"),
            Value::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Num(Rational64),
    Bool(bool),
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Num(num) => write!(f, "{}", num),
            Lit::Bool(b) => write!(f, "{}", b),
        }
    }
}

pub fn eval<'src>(
    src: &'src str,
    repl_src: &'src str,
    env: Rc<RefCell<Env>>,
    root: &Node<Root>,
) -> RuntimeResult<Value> {
    let mut val = Value::Unit;
    for item in &root.items {
        match item.inner().clone() {
            Item::Expr(expr) => {
                val = eval_expr(src, repl_src, env.clone(), Node::new(expr, item.span()))?;
            }
            Item::Def { name, expr, .. } => {
                let value = eval_expr(src, repl_src, env.clone(), expr)?;
                env.borrow_mut().insert(name.inner().clone(), value);
                val = Value::Unit;
            }
            Item::Fn {
                name,
                params,
                body,
                ty,
            } => {
                let value = Value::Lambda {
                    env: env.clone(),
                    params,
                    body: body.clone(),
                };
                env.borrow_mut().insert(name.inner().clone(), value.clone());
                // println!("val: {:?}", value);
                val = Value::Unit;
            }
        }
    }
    Ok(val)
}

fn eval_expr<'src>(
    src: &'src str,
    repl_src: &'src str,
    mut env: Rc<RefCell<Env>>,
    mut expr: Node<Expr>,
) -> RuntimeResult<Value> {
    let val: Value;
    'tco: loop {
        val = match expr.inner().clone() {
            Expr::Lit { lit, .. } => match lit {
                infer::Lit::Num(num) => Value::Lit(Lit::Num(num.clone())),
                infer::Lit::Bool(b) => Value::Lit(Lit::Bool(b)),
            },
            Expr::Ident { name, .. } => {
                if let Some(value) = env.borrow().get(&name) {
                    value
                } else {
                    return Err(
                        format!("Identifier '{}' not found", repl_src[name.span()].trim()).into(),
                    );
                }
            }
            Expr::Lambda { params, body, .. } => Value::Lambda {
                env: Env::new_with_parent(env.clone()),
                params: params.clone(),
                body: body.clone(),
            },
            Expr::Apply { fun, args, .. } => {
                let fun = eval_expr(src, repl_src, env.clone(), fun)?;
                // println!("fun: {:?}", fun);
                match fun {
                    Value::Lambda {
                        env: lam_env,
                        params,
                        body,
                    } => {
                        let arg_env = Env::new_with_parent(lam_env.clone());
                        for (param, arg) in params.iter().zip(args) {
                            arg_env.borrow_mut().insert(
                                param.inner().clone(),
                                eval_expr(src, repl_src, env.clone(), arg)?,
                            );
                        }
                        // println!("arg_env: {:#?}", arg_env.borrow());

                        expr = body;
                        env = arg_env;
                        continue 'tco;
                    }
                    Value::NativeFn(fun) => fun(args
                        .into_iter()
                        .map(|e| eval_expr(src, repl_src, env.clone(), e))
                        .try_collect()?)?,
                    _ => {
                        return Err(format!("Expected lambda, found {:?}", fun).into());
                    }
                }
            }
            Expr::Let {
                name, expr, body, ..
            } => {
                let value = eval_expr(src, repl_src, env.clone(), expr)?;
                env.borrow_mut().insert(name.inner().clone(), value);
                eval_expr(src, repl_src, env, body)?
            }
            Expr::Fn {
                name,
                params,
                expr,
                body,
                ty,
            } => {
                let value = Value::Lambda {
                    env: Env::new_with_parent(env.clone()),
                    params,
                    body: expr.clone(),
                };
                env.borrow_mut().insert(name.inner().clone(), value);
                eval_expr(src, repl_src, env, body)?
            }
            Expr::If {
                cond, then, else_, ..
            } => {
                let cond = eval_expr(src, repl_src, env.clone(), cond)?;
                match cond {
                    Value::Lit(Lit::Bool(true)) => {
                        expr = then;
                        continue 'tco;
                    }
                    Value::Lit(Lit::Bool(false)) => {
                        expr = else_;
                        continue 'tco;
                    }
                    _ => {
                        return Err(format!("Expected bool, found {:?}", cond).into());
                    }
                }
            }
            Expr::Unit => Value::Unit,
        };

        break 'tco;
    }
    Ok(val)
}
