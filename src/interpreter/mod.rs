use crate::{
    intern::InternedString,
    list::List,
    parser::ast::{Decl, Expr, FnExpr, InfixOp, LetExpr, Lit, MatchArm, Pattern, PrefixOp},
};
use num_bigint::BigInt;
use num_complex::Complex64;
use num_rational::BigRational;
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

pub mod repl;
mod test;

#[derive(Debug, Clone, PartialEq)]
pub struct Env {
    pub bindings: HashMap<InternedString, Value>,
    pub parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    pub fn create_child(parent: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(Self {
            bindings: HashMap::new(),
            parent: Some(parent),
        }))
    }

    pub fn define(&mut self, name: InternedString, val: Value) {
        self.bindings.insert(name, val);
    }

    pub fn lookup(&self, name: &InternedString) -> Option<Value> {
        if let Some(v) = self.bindings.get(name) {
            Some(v.clone())
        } else if let Some(parent) = &self.parent {
            parent.as_ref().borrow().lookup(name)
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(Int),
    Rational(BigRational),
    Real(f64),
    Complex(Complex64),
    String(InternedString),
    Char(char),
    Bool(bool),
    List(List<Self>),
    Tuple(Vec<Self>),
    Map(HashMap<InternedString, Self>),
    Record(Record),
    Lambda(Lambda),
    Unit,
}

impl Value {
    pub fn int(&self) -> Option<Int> {
        match self {
            Self::Int(i) => Some(i.clone()),
            _ => None,
        }
    }

    pub fn rational(&self) -> Option<BigRational> {
        match self {
            Self::Rational(r) => Some(r.clone()),
            _ => None,
        }
    }

    pub fn real(&self) -> Option<f64> {
        match self {
            Self::Real(r) => Some(*r),
            _ => None,
        }
    }

    pub fn complex(&self) -> Option<Complex64> {
        match self {
            Self::Complex(c) => Some(*c),
            _ => None,
        }
    }

    pub fn string(&self) -> Option<InternedString> {
        match self {
            Self::String(s) => Some(s.clone()),
            _ => None,
        }
    }

    pub fn char(&self) -> Option<char> {
        match self {
            Self::Char(c) => Some(*c),
            _ => None,
        }
    }

    pub fn bool(&self) -> Option<bool> {
        match self {
            Self::Bool(b) => Some(*b),
            _ => None,
        }
    }

    pub fn list(&self) -> Option<List<Self>> {
        match self {
            Self::List(l) => Some(l.clone()),
            _ => None,
        }
    }

    pub fn tuple(&self) -> Option<Vec<Self>> {
        match self {
            Self::Tuple(t) => Some(t.clone()),
            _ => None,
        }
    }

    pub fn map(&self) -> Option<HashMap<InternedString, Self>> {
        match self {
            Self::Map(m) => Some(m.clone()),
            _ => None,
        }
    }

    pub fn record(&self) -> Option<Record> {
        match self {
            Self::Record(r) => Some(r.clone()),
            _ => None,
        }
    }

    pub fn lambda(&self) -> Option<Lambda> {
        match self {
            Self::Lambda(l) => Some(l.clone()),
            _ => None,
        }
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(i) => write!(f, "{}", i),
            Value::Rational(r) => write!(f, "{}", r),
            Value::Real(r) => write!(f, "{}", r),
            Value::Complex(c) => write!(f, "{}", c),
            Value::String(s) => write!(f, "{}", s),
            Value::Char(c) => write!(f, "{}", c),
            Value::Bool(b) => write!(f, "{}", b),
            Value::List(l) => write!(f, "{}", l),
            Value::Tuple(t) => write!(f, "{:?}", t),
            Value::Map(m) => write!(f, "{:?}", m),
            Value::Record(r) => write!(f, "{:?}", r),
            Value::Lambda(l) => write!(f, "{:?}", l),
            Value::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Int {
    pub value: BigInt,
}

impl Display for Int {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    pub fields: HashMap<InternedString, Value>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub env: Rc<RefCell<Env>>,
    pub param: Pattern,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RuntimeError(pub String);

impl RuntimeError {
    pub fn new(msg: &str) -> Self {
        Self(msg.to_string())
    }
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type Result<T> = std::result::Result<T, RuntimeError>;

pub fn eval(env: Rc<RefCell<Env>>, expr: &Expr) -> Result<Value> {
    match expr.clone() {
        Expr::Ident(name) => env
            .borrow()
            .lookup(&name)
            .ok_or(RuntimeError(format!("undefined variable: {}", name))),
        Expr::Lit(l) => match l {
            Lit::Int(l) => Ok(Value::Int(Int { value: l })),
            Lit::Rational(r) => Ok(Value::Rational(r)),
            Lit::Real(r) => Ok(Value::Real(r.0)),
            Lit::Complex(c) => Ok(Value::Complex(c)),
            Lit::String(s) => Ok(Value::String(s)),
            Lit::Char(c) => Ok(Value::Char(c)),
            Lit::Bool(b) => Ok(Value::Bool(b)),
            Lit::List(l) => Ok(Value::List(
                l.map(|e| eval(env.clone(), &e)).collect::<Result<_>>()?,
            )),
            Lit::Tuple(t) => Ok(Value::Tuple(
                t.into_iter()
                    .map(|e| eval(env.clone(), &e))
                    .collect::<Result<_>>()?,
            )),
            Lit::Map(m) => Ok(Value::Map(
                m.into_iter()
                    .map(|(k, v)| Ok((k, eval(env.clone(), &v)?)))
                    .collect::<Result<_>>()?,
            )),
            Lit::Record(r) => Ok(Value::Record(Record {
                fields: r
                    .into_iter()
                    .map(|(k, v)| Ok((k, eval(env.clone(), &v)?)))
                    .collect::<Result<_>>()?,
            })),
            Lit::Lambda(l) => Ok(Value::Lambda(Lambda {
                env: Env::create_child(env.clone()),
                param: l.param,
                body: l.body,
            })),
        },
        Expr::Prefix { op, expr } => match op {
            PrefixOp::Neg => match eval(env.clone(), &*expr)? {
                Value::Int(i) => Ok(Value::Int(Int {
                    value: -i.value.clone(),
                })),
                Value::Real(r) => Ok(Value::Real(-r)),
                Value::Complex(c) => Ok(Value::Complex(-c)),
                _ => Err(RuntimeError::new("cannot negate non-numeric values")),
            },
            PrefixOp::Not => match eval(env.clone(), &*expr)? {
                Value::Bool(b) => Ok(Value::Bool(!b)),
                _ => Err(RuntimeError::new("cannot negate non-boolean values")),
            },
        },
        Expr::Infix { op, lhs, rhs } => match op {
            InfixOp::Add => match (eval(env.clone(), &*lhs)?, eval(env.clone(), &*rhs)?) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(Int {
                    value: l.value + r.value,
                })),
                (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l + r)),
                (Value::Complex(l), Value::Complex(r)) => Ok(Value::Complex(l + r)),
                _ => Err(RuntimeError::new("cannot add non-numeric values")),
            },
            InfixOp::Sub => match (eval(env.clone(), &*lhs)?, eval(env.clone(), &*rhs)?) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(Int {
                    value: l.value - r.value,
                })),
                (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l - r)),
                (Value::Complex(l), Value::Complex(r)) => Ok(Value::Complex(l - r)),
                _ => Err(RuntimeError::new("cannot subtract non-numeric values")),
            },
            InfixOp::Mul => match (eval(env.clone(), &*lhs)?, eval(env.clone(), &*rhs)?) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(Int {
                    value: l.value * r.value,
                })),
                (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l * r)),
                (Value::Complex(l), Value::Complex(r)) => Ok(Value::Complex(l * r)),
                _ => Err(RuntimeError::new("cannot multiply non-numeric values")),
            },
            InfixOp::Div => match (eval(env.clone(), &*lhs)?, eval(env.clone(), &*rhs)?) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Int(Int {
                    value: l.value / r.value,
                })),
                (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l / r)),
                (Value::Complex(l), Value::Complex(r)) => Ok(Value::Complex(l / r)),
                _ => Err(RuntimeError::new("cannot divide non-numeric values")),
            },
            InfixOp::Mod => {
                let (lv, rv) = (eval(env.clone(), &*lhs)?, eval(env.clone(), &*rhs)?);
                match (lv.clone(), rv.clone()) {
                    (Value::Int(l), Value::Int(r)) => Ok(Value::Int(Int {
                        value: l.value % r.value,
                    })),
                    _ => Err(RuntimeError(format!(
                        "cannot modulo non-integer values {:?} % {:?}",
                        lv, rv
                    ))),
                }
            }
            InfixOp::Pow => match (eval(env.clone(), &*lhs)?, eval(env.clone(), &*rhs)?) {
                (Value::Int(l), Value::Int(r)) => r
                    .value
                    .to_str_radix(10)
                    .parse()
                    .map(|rhs| {
                        Ok(Value::Int(Int {
                            value: l.value.pow(rhs),
                        }))
                    })
                    .unwrap_or_else(|_| {
                        Err(RuntimeError::new("cannot exponentiate non-integer values"))
                    }),
                (Value::Real(l), Value::Real(r)) => Ok(Value::Real(l.powf(r))),
                (Value::Complex(l), Value::Complex(r)) => Ok(Value::Complex(l.powc(r))),
                _ => Err(RuntimeError::new("cannot exponentiate non-numeric values")),
            },
            InfixOp::Eq => match (eval(env.clone(), &*lhs)?, eval(env.clone(), &*rhs)?) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l.value == r.value)),
                (Value::Real(l), Value::Real(r)) => Ok(Value::Bool(l == r)),
                (Value::Complex(l), Value::Complex(r)) => Ok(Value::Bool(l == r)),
                (Value::String(l), Value::String(r)) => Ok(Value::Bool(l == r)),
                (Value::Char(l), Value::Char(r)) => Ok(Value::Bool(l == r)),
                (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l == r)),
                (Value::List(l), Value::List(r)) => Ok(Value::Bool(l == r)),
                (Value::Tuple(l), Value::Tuple(r)) => Ok(Value::Bool(l == r)),
                (Value::Map(l), Value::Map(r)) => Ok(Value::Bool(l == r)),
                (Value::Record(l), Value::Record(r)) => Ok(Value::Bool(l == r)),
                (Value::Lambda(l), Value::Lambda(r)) => Ok(Value::Bool(l == r)),
                (Value::Unit, Value::Unit) => Ok(Value::Bool(true)),
                _ => Ok(Value::Bool(false)),
            },
            InfixOp::Neq => match (eval(env.clone(), &*lhs)?, eval(env.clone(), &*rhs)?) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l.value != r.value)),
                (Value::Real(l), Value::Real(r)) => Ok(Value::Bool(l != r)),
                (Value::Complex(l), Value::Complex(r)) => Ok(Value::Bool(l != r)),
                (Value::String(l), Value::String(r)) => Ok(Value::Bool(l != r)),
                (Value::Char(l), Value::Char(r)) => Ok(Value::Bool(l != r)),
                (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l != r)),
                (Value::List(l), Value::List(r)) => Ok(Value::Bool(l != r)),
                (Value::Tuple(l), Value::Tuple(r)) => Ok(Value::Bool(l != r)),
                (Value::Map(l), Value::Map(r)) => Ok(Value::Bool(l != r)),
                (Value::Record(l), Value::Record(r)) => Ok(Value::Bool(l != r)),
                (Value::Lambda(l), Value::Lambda(r)) => Ok(Value::Bool(l != r)),
                (Value::Unit, Value::Unit) => Ok(Value::Bool(true)),
                _ => Ok(Value::Bool(false)),
            },
            InfixOp::Lss => match (eval(env.clone(), &*lhs)?, eval(env.clone(), &*rhs)?) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l.value < r.value)),
                (Value::Real(l), Value::Real(r)) => Ok(Value::Bool(l < r)),
                (Value::Complex(l), Value::Complex(r)) => Ok(Value::Bool(l.norm() < r.norm())),
                _ => Err(RuntimeError::new("cannot compare non-numeric values")),
            },
            InfixOp::Gtr => match (eval(env.clone(), &*lhs)?, eval(env.clone(), &*rhs)?) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l.value > r.value)),
                (Value::Real(l), Value::Real(r)) => Ok(Value::Bool(l > r)),
                (Value::Complex(l), Value::Complex(r)) => Ok(Value::Bool(l.norm() > r.norm())),
                _ => Err(RuntimeError::new("cannot compare non-numeric values")),
            },
            InfixOp::Leq => match (eval(env.clone(), &*lhs)?, eval(env.clone(), &*rhs)?) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l.value <= r.value)),
                (Value::Real(l), Value::Real(r)) => Ok(Value::Bool(l <= r)),
                (Value::Complex(l), Value::Complex(r)) => Ok(Value::Bool(l.norm() <= r.norm())),
                _ => Err(RuntimeError::new("cannot compare non-numeric values")),
            },
            InfixOp::Geq => match (eval(env.clone(), &*lhs)?, eval(env.clone(), &*rhs)?) {
                (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l.value >= r.value)),
                (Value::Real(l), Value::Real(r)) => Ok(Value::Bool(l >= r)),
                (Value::Complex(l), Value::Complex(r)) => Ok(Value::Bool(l.norm() >= r.norm())),
                _ => Err(RuntimeError::new("cannot compare non-numeric values")),
            },
            InfixOp::And => match (eval(env.clone(), &*lhs)?, eval(env.clone(), &*rhs)?) {
                (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l && r)),
                _ => Err(RuntimeError::new("cannot and non-boolean values")),
            },
            InfixOp::Or => match (eval(env.clone(), &*lhs)?, eval(env.clone(), &*rhs)?) {
                (Value::Bool(l), Value::Bool(r)) => Ok(Value::Bool(l || r)),
                _ => Err(RuntimeError::new("cannot or non-boolean values")),
            },
        },
        Expr::Let(LetExpr {
            pattern,
            value,
            body,
        }) => {
            let val = eval(env.clone(), &value)?;
            let ds = destructure_pattern(&pattern, &val)
                .ok_or(RuntimeError::new("failed to destructure value"))?;
            let let_env = Env::create_child(env.clone());
            for (name, value) in ds.bindings {
                let_env.borrow_mut().define(name, value);
            }
            eval(let_env, &*body)
        }
        Expr::Fn(FnExpr { name, value, body }) => match eval(env.clone(), &*value)? {
            Value::Lambda(Lambda {
                env: lambda_env,
                param,
                body: lam_body,
            }) => {
                lambda_env.borrow_mut().define(
                    name.clone(),
                    Value::Lambda(Lambda {
                        env: lambda_env.clone(),
                        param: param.clone(),
                        body: lam_body.clone(),
                    }),
                );
                let lam = eval(lambda_env.clone(), &value)?;
                let body_env = Env::create_child(env.clone());
                body_env.borrow_mut().define(name, lam);
                eval(body_env.clone(), &*body)
            }
            _ => Err(RuntimeError::new("cannot define non-lambda value")),
        },
        Expr::Apply(fun, arg) => match eval(env.clone(), &fun)? {
            Value::Lambda(Lambda {
                env: lambda_env,
                param,
                body,
            }) => {
                let varg = eval(env.clone(), &arg)?;
                let ds = destructure_pattern(&param, &varg)
                    .ok_or(RuntimeError::new("failed to destructure argument"))?;
                let arg_env = Env::create_child(lambda_env.clone());
                for (name, value) in ds.bindings.clone() {
                    arg_env.borrow_mut().define(name, value);
                }
                eval(arg_env.clone(), &*body)
            }
            _ => Err(RuntimeError::new("cannot apply non-lambda value")),
        },
        Expr::If { cond, then, else_ } => {
            if eval(env.clone(), &*cond)? == Value::Bool(true) {
                eval(env.clone(), &*then)
            } else {
                eval(env.clone(), &*else_)
            }
        }
        Expr::Match { expr, arms } => match eval(env.clone(), &*expr)? {
            Value::Int(i) => arms
                .iter()
                .filter(|MatchArm { pattern, expr: _ }| {
                    destructure_pattern(pattern, &Value::Int(i.clone())).is_some()
                })
                .next()
                .map(
                    |MatchArm {
                         pattern: _,
                         expr: e,
                     }| eval(env.clone(), &*e),
                )
                .unwrap_or_else(|| Err(RuntimeError::new("failed to match value"))),
            Value::Rational(r) => arms
                .iter()
                .filter(|MatchArm { pattern, expr: _ }| {
                    destructure_pattern(pattern, &Value::Rational(r.clone())).is_some()
                })
                .next()
                .map(
                    |MatchArm {
                         pattern: _,
                         expr: e,
                     }| eval(env.clone(), &*e),
                )
                .unwrap_or_else(|| Err(RuntimeError::new("failed to match value"))),
            Value::Real(r) => arms
                .iter()
                .filter(|MatchArm { pattern, expr: _ }| {
                    destructure_pattern(pattern, &Value::Real(r.clone())).is_some()
                })
                .next()
                .map(
                    |MatchArm {
                         pattern: _,
                         expr: e,
                     }| eval(env.clone(), &*e),
                )
                .unwrap_or_else(|| Err(RuntimeError::new("failed to match value"))),
            Value::Complex(c) => arms
                .iter()
                .filter(|MatchArm { pattern, expr: _ }| {
                    destructure_pattern(pattern, &Value::Complex(c.clone())).is_some()
                })
                .next()
                .map(
                    |MatchArm {
                         pattern: _,
                         expr: e,
                     }| eval(env.clone(), &*e),
                )
                .unwrap_or_else(|| Err(RuntimeError::new("failed to match value"))),
            Value::String(s) => arms
                .iter()
                .filter(|MatchArm { pattern, expr: _ }| {
                    destructure_pattern(pattern, &Value::String(s.clone())).is_some()
                })
                .next()
                .map(
                    |MatchArm {
                         pattern: _,
                         expr: e,
                     }| eval(env.clone(), &*e),
                )
                .unwrap_or_else(|| Err(RuntimeError::new("failed to match value"))),
            Value::Char(c) => arms
                .iter()
                .filter(|MatchArm { pattern, expr: _ }| {
                    destructure_pattern(pattern, &Value::Char(c.clone())).is_some()
                })
                .next()
                .map(
                    |MatchArm {
                         pattern: _,
                         expr: e,
                     }| eval(env.clone(), &*e),
                )
                .unwrap_or_else(|| Err(RuntimeError::new("failed to match value"))),
            Value::Bool(b) => arms
                .iter()
                .filter(|MatchArm { pattern, expr: _ }| {
                    destructure_pattern(pattern, &Value::Bool(b.clone())).is_some()
                })
                .next()
                .map(
                    |MatchArm {
                         pattern: _,
                         expr: e,
                     }| eval(env.clone(), &*e),
                )
                .unwrap_or_else(|| Err(RuntimeError::new("failed to match value"))),
            Value::List(l) => arms
                .iter()
                .filter(|MatchArm { pattern, expr: _ }| {
                    destructure_pattern(pattern, &Value::List(l.clone())).is_some()
                })
                .next()
                .map(
                    |MatchArm {
                         pattern: _,
                         expr: e,
                     }| eval(env.clone(), &*e),
                )
                .unwrap_or_else(|| Err(RuntimeError::new("failed to match value"))),
            Value::Tuple(t) => arms
                .iter()
                .filter(|MatchArm { pattern, expr: _ }| {
                    destructure_pattern(pattern, &Value::Tuple(t.clone())).is_some()
                })
                .next()
                .map(
                    |MatchArm {
                         pattern: _,
                         expr: e,
                     }| eval(env.clone(), &*e),
                )
                .unwrap_or_else(|| Err(RuntimeError::new("failed to match value"))),
            Value::Map(_) => todo!(),
            Value::Record(r) => arms
                .iter()
                .filter(|MatchArm { pattern, expr: _ }| {
                    destructure_pattern(pattern, &Value::Record(r.clone())).is_some()
                })
                .next()
                .map(
                    |MatchArm {
                         pattern: _,
                         expr: e,
                     }| eval(env.clone(), &*e),
                )
                .unwrap_or_else(|| Err(RuntimeError::new("failed to match value"))),
            Value::Lambda(_) => todo!(),
            Value::Unit => todo!(),
        },
        Expr::Unit => Ok(Value::Unit),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DestructureResult {
    pub bindings: HashMap<InternedString, Value>,
    pub rest: Option<Value>,
}

// Destructure a pattern against a value.
fn destructure_pattern(pattern: &Pattern, value: &Value) -> Option<DestructureResult> {
    match pattern {
        Pattern::Ident(name) => {
            let mut bindings = HashMap::new();
            bindings.insert(name.clone(), value.clone());
            Some(DestructureResult {
                bindings,
                rest: None,
            })
        }
        Pattern::Int(i) => value.int().map(|z| {
            if *i == z.value {
                DestructureResult {
                    bindings: HashMap::new(),
                    rest: Some(Value::Int(z.clone())),
                }
            } else {
                DestructureResult {
                    bindings: HashMap::new(),
                    rest: None,
                }
            }
        }),
        Pattern::Rational(r) => value.rational().map(|z| {
            if r == &z {
                DestructureResult {
                    bindings: HashMap::new(),
                    rest: Some(Value::Rational(z.clone())),
                }
            } else {
                DestructureResult {
                    bindings: HashMap::new(),
                    rest: None,
                }
            }
        }),
        Pattern::Bool(b) => value.bool().map(|z| {
            if b == &z {
                DestructureResult {
                    bindings: HashMap::new(),
                    rest: Some(Value::Bool(z.clone())),
                }
            } else {
                DestructureResult {
                    bindings: HashMap::new(),
                    rest: None,
                }
            }
        }),
        Pattern::String(s) => value.string().map(|z| {
            if s == &z {
                DestructureResult {
                    bindings: HashMap::new(),
                    rest: Some(Value::String(z.clone())),
                }
            } else {
                DestructureResult {
                    bindings: HashMap::new(),
                    rest: None,
                }
            }
        }),
        Pattern::Char(c) => value.char().map(|z| {
            if c == &z {
                DestructureResult {
                    bindings: HashMap::new(),
                    rest: Some(Value::Char(z.clone())),
                }
            } else {
                DestructureResult {
                    bindings: HashMap::new(),
                    rest: None,
                }
            }
        }),
        Pattern::List(l) => {
            if let Some(v) = value.list() {
                if v.len() == l.items.len() {
                    let mut bindings = HashMap::new();
                    for (p, v) in l.items.iter().zip(v) {
                        if let Some(ds) = destructure_pattern(p, &v) {
                            for (name, value) in ds.bindings {
                                bindings.insert(name, value);
                            }
                        } else {
                            return None;
                        }
                    }
                    Some(DestructureResult {
                        bindings,
                        rest: None,
                    })
                } else {
                    None
                }
            } else {
                None
            }
        }
        Pattern::Tuple(t) => {
            if let Some(v) = value.tuple() {
                if v.len() == t.items.len() {
                    let mut bindings = HashMap::new();
                    for (p, v) in t.items.iter().zip(v) {
                        if let Some(ds) = destructure_pattern(p, &v) {
                            for (name, value) in ds.bindings {
                                bindings.insert(name, value);
                            }
                        } else {
                            return None;
                        }
                    }
                    Some(DestructureResult {
                        bindings,
                        rest: None,
                    })
                } else {
                    None
                }
            } else {
                None
            }
        }
        Pattern::Map(_) => todo!(),
        Pattern::Record(r) => {
            todo!()
        }
        Pattern::Wildcard => Some(DestructureResult {
            bindings: HashMap::new(),
            rest: None,
        }),
        Pattern::Unit => todo!(),
    }
}

pub fn handle_decl(env: Rc<RefCell<Env>>, decl: &Decl) -> Result<()> {
    match decl {
        Decl::Let(l) => {
            let val = eval(env.clone(), &l.value)?;
            let ds = destructure_pattern(&l.pattern, &val)
                .ok_or(RuntimeError::new("failed to destructure value"))?;
            for (name, value) in ds.bindings {
                env.borrow_mut().define(name, value);
            }
            Ok(())
        }
        Decl::Fn(f) => match &eval(env.clone(), &f.value)? {
            val @ Value::Lambda(Lambda {
                env: lambda_env,
                param,
                body,
            }) => {
                lambda_env.borrow_mut().define(f.name.clone(), val.clone());
                let lam = eval(lambda_env.clone(), &f.value)?;
                let body_env = Env::create_child(env.clone());
                body_env.borrow_mut().define(f.name.clone(), lam);
                env.borrow_mut().define(
                    f.name.clone(),
                    Value::Lambda(Lambda {
                        env: body_env.clone(),
                        param: param.clone(),
                        body: body.clone(),
                    }),
                );
                Ok(())
            }
            _ => Err(RuntimeError::new(
                "cannot define non-lambda value with `fn`",
            )),
        },
    }
}
