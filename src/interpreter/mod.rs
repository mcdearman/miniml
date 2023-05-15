use crate::{
    intern::InternedString,
    list::List,
    parser::{Expr, Int, Lit, Pattern, PrefixOp},
};
use num_bigint::BigInt;
use num_complex::Complex64;
use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

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
    Real(f64),
    Complex(Complex64),
    String(InternedString),
    Char(char),
    Bool(bool),
    List(List<Self>),
    Tuple(Vec<Self>),
    Map(HashMap<InternedString, Self>),
    Record(HashMap<InternedString, Self>),
    Lambda {
        env: Rc<RefCell<Env>>,
        param: Pattern,
        body: Box<Expr>,
    },
    Unit,
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
            Lit::Int(l) => Ok(Value::Int(l)),
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
            Lit::Map(m) => todo!(),
            Lit::Record(r) => todo!(),
            Lit::Lambda(l) => Ok(Value::Lambda {
                env: Env::create_child(env.clone()),
                param: l.param,
                body: l.body,
            }),
        },
        Expr::Prefix { op, expr } => match op {
            PrefixOp::Neg => todo!(),
            PrefixOp::Not => todo!(),
        },
        Expr::Infix { op, lhs, rhs } => todo!(),
        Expr::Let(_) => todo!(),
        Expr::Apply(fun, arg) => match eval(env.clone(), &fun)? {
            _ => todo!()
            // Value::Lambda {
            //     env: lambda_env,
            //     param,
            //     body,
            // } => {
            //     let arg = eval(env.clone(), &arg)?;
            //     lambda_env.borrow_mut().define(param, arg);
            //     eval(lambda_env.clone(), &*body)
            // }
            // _ => Err(RuntimeError::new("cannot apply non-lambda value")),
        },
        Expr::If { cond, then, else_ } => todo!(),
        Expr::Match { expr, arms } => todo!(),
        Expr::Unit => Ok(Value::Unit),
    }
}

fn pattern_matches(pattern: &Pattern, val: &Value) -> bool {
    match pattern {
        Pattern::Ident(name) => todo!(),
        Pattern::Int(_) => todo!(),
        Pattern::BigInt(_) => todo!(),
        Pattern::Rational(_) => todo!(),
        Pattern::Bool(_) => todo!(),
        Pattern::String(_) => todo!(),
        Pattern::Char(_) => todo!(),
        Pattern::List(_) => todo!(),
        Pattern::Tuple(_) => todo!(),
        Pattern::Map(_) => todo!(),
        Pattern::Record(_) => todo!(),
        Pattern::Wildcard => true,
        Pattern::Unit => todo!(),
    }
}
