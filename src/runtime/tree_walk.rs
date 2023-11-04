use crate::{
    analysis::infer::{self, Expr, Item, Root},
    util::{intern::InternedString, node::SrcNode, unique_id::UniqueId},
};
use num_rational::Rational64;
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

pub type RuntimeError = InternedString;
pub type RuntimeResult<T> = Result<T, RuntimeError>;

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Lit(Lit),
    Lambda {
        env: Rc<RefCell<Env>>,
        params: Vec<SrcNode<UniqueId>>,
        body: SrcNode<Expr>,
    },
    Unit,
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Lit(Lit::Num(num)) => write!(f, "{}", num),
            Value::Lit(Lit::Bool(b)) => write!(f, "{}", b),
            Value::Lambda { .. } => write!(f, "<lambda>"),
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
    env: Rc<RefCell<Env>>,
    root: &SrcNode<Root>,
) -> RuntimeResult<Value> {
    let mut val = Value::Unit;
    for item in &root.items {
        match item.inner().clone() {
            Item::Expr(expr) => {
                val = eval_expr(src, env.clone(), SrcNode::new(expr, item.span()))?;
            }
            Item::Def { name, expr } => {
                let value = eval_expr(src, env.clone(), expr)?;
                env.borrow_mut().insert(name.inner().clone(), value);
                val = Value::Unit;
            }
        }
    }
    Ok(val)
}

fn eval_expr<'src>(
    src: &'src str,
    mut env: Rc<RefCell<Env>>,
    mut expr: SrcNode<Expr>,
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
                        format!("Identifier '{}' not found", src[name.span()].trim()).into(),
                    );
                }
            }
            Expr::Lambda { params, body, .. } => Value::Lambda {
                env: Env::new_with_parent(env.clone()),
                params: params.clone(),
                body: body.clone(),
            },
            Expr::Apply { fun, args, .. } => {
                let fun = eval_expr(src, env.clone(), fun)?;
                match fun {
                    Value::Lambda {
                        env: lam_env,
                        params,
                        body,
                    } => {
                        for (param, arg) in params.iter().zip(args) {
                            lam_env
                                .borrow_mut()
                                .insert(param.inner().clone(), eval_expr(src, env.clone(), arg)?);
                        }
                        // eval_expr(src, env.clone(), &body)?

                        expr = body;
                        env = lam_env;
                        continue 'tco;
                    }
                    _ => {
                        return Err(format!("Expected lambda, found {:?}", fun).into());
                    }
                }
            }
            Expr::Let {
                name, expr, body, ..
            } => {
                let value = eval_expr(src, env.clone(), expr)?;
                env.borrow_mut().insert(name.inner().clone(), value);
                eval_expr(src, env, body)?
            }
            Expr::Infix { op, lhs, rhs, .. } => match op.inner() {
                infer::InfixOp::Add => {
                    let lhs = eval_expr(src, env.clone(), lhs)?;
                    let rhs = eval_expr(src, env, rhs)?;
                    match (&lhs, &rhs) {
                        (&Value::Lit(Lit::Num(lhs)), &Value::Lit(Lit::Num(rhs))) => {
                            Value::Lit(Lit::Num(lhs + rhs))
                        }
                        _ => {
                            return Err(
                                format!("Expected numbers, found {:?} and {:?}", lhs, rhs).into()
                            );
                        }
                    }
                }
                infer::InfixOp::Sub => {
                    let lhs = eval_expr(src, env.clone(), lhs)?;
                    let rhs = eval_expr(src, env, rhs)?;
                    match (&lhs, &rhs) {
                        (&Value::Lit(Lit::Num(lhs)), &Value::Lit(Lit::Num(rhs))) => {
                            Value::Lit(Lit::Num(lhs - rhs))
                        }
                        _ => {
                            return Err(
                                format!("Expected numbers, found {:?} and {:?}", lhs, rhs).into()
                            );
                        }
                    }
                }
                infer::InfixOp::Mul => {
                    let lhs = eval_expr(src, env.clone(), lhs)?;
                    let rhs = eval_expr(src, env, rhs)?;
                    match (&lhs, &rhs) {
                        (&Value::Lit(Lit::Num(lhs)), &Value::Lit(Lit::Num(rhs))) => {
                            Value::Lit(Lit::Num(lhs * rhs))
                        }
                        _ => {
                            return Err(
                                format!("Expected numbers, found {:?} and {:?}", lhs, rhs).into()
                            );
                        }
                    }
                }
                infer::InfixOp::Div => {
                    let lhs = eval_expr(src, env.clone(), lhs)?;
                    let rhs = eval_expr(src, env, rhs)?;
                    match (&lhs, &rhs) {
                        (&Value::Lit(Lit::Num(lhs)), &Value::Lit(Lit::Num(rhs))) => {
                            Value::Lit(Lit::Num(lhs / rhs))
                        }
                        _ => {
                            return Err(
                                format!("Expected numbers, found {:?} and {:?}", lhs, rhs).into()
                            );
                        }
                    }
                }
            },

            Expr::Prefix { op, expr, .. } => match op.inner() {
                infer::PrefixOp::Neg => {
                    let val = eval_expr(src, env, expr)?;
                    match val {
                        Value::Lit(Lit::Num(num)) => Value::Lit(Lit::Num(-num)),
                        _ => {
                            return Err(format!("Expected number, found {:?}", val).into());
                        }
                    }
                }
                infer::PrefixOp::Not => {
                    let val = eval_expr(src, env, expr)?;
                    match val {
                        Value::Lit(Lit::Bool(b)) => Value::Lit(Lit::Bool(!b)),
                        _ => {
                            return Err(format!("Expected bool, found {:?}", val).into());
                        }
                    }
                }
            },
            Expr::Unit => Value::Unit,
        };

        break 'tco;
    }
    Ok(val)
}
