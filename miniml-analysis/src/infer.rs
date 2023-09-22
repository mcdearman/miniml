use miniml_syntax::ast::{Expr, InfixOp};
use miniml_util::intern::InternedString;
use std::{
    collections::{BTreeSet, HashMap},
    fmt::{Debug, Display},
    sync::atomic::{AtomicUsize, Ordering},
};

const ARITHMETIC_TYPES: &[&[Type]] = &[
    &[Type::Nat, Type::Nat, Type::Nat],
    &[Type::Int, Type::Int, Type::Int],
    &[Type::Rational, Type::Rational, Type::Rational],
    &[Type::Real, Type::Real, Type::Real],
    &[Type::Complex, Type::Complex, Type::Complex],
];

const ALLOWED_TYPES: &[&[&[Type]]] = &[
    // +, -, *, /
    ARITHMETIC_TYPES,
    ARITHMETIC_TYPES,
    ARITHMETIC_TYPES,
    ARITHMETIC_TYPES,
    // rem
    &[&[Type::Nat, Type::Nat, Type::Nat]],
    // pow
    &[
        &[Type::Nat, Type::Nat, Type::Nat],
        &[Type::Int, Type::Nat, Type::Int],
        &[Type::Rational, Type::Nat, Type::Rational],
        &[Type::Real, Type::Nat, Type::Real],
        &[Type::Complex, Type::Nat, Type::Complex],
    ],
];

#[derive(Clone, PartialEq)]
pub enum Type {
    Nat,
    Int,
    Rational,
    Real,
    Complex,
    String,
    Char,
    Bool,
    Var(TyVar),
    Lambda { params: Vec<Self>, body: Box<Self> },
}

impl Type {
    fn lower(&self, vars: &mut HashMap<TyVar, TyVar>) -> Self {
        match self.clone() {
            Self::Var(name) => {
                if let Some(n) = vars.get(&name) {
                    Self::Var(*n)
                } else {
                    let n = vars.len();
                    vars.insert(name, TyVar(n));
                    Self::Var(TyVar(n))
                }
            }
            Self::Lambda { params, body } => Self::Lambda {
                params: params.into_iter().fold(vec![], |mut acc, param| {
                    acc.push(param.lower(vars));
                    acc
                }),
                body: Box::new(body.lower(vars)),
            },
            t => t,
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Self::Nat => write!(f, "Nat"),
            Self::Int => write!(f, "Int"),
            Self::Rational => write!(f, "Rational"),
            Self::Real => write!(f, "Real"),
            Self::Complex => write!(f, "Complex"),
            Self::String => write!(f, "String"),
            Self::Char => write!(f, "Char"),
            Self::Bool => write!(f, "Bool"),
            Self::Var(n) => write!(f, "{:?}", n),
            Self::Lambda { params, body } => {
                let mut params = params
                    .into_iter()
                    .map(|param| format!("{:?}", param))
                    .collect::<Vec<_>>()
                    .join(", ");
                if params.len() > 1 {
                    params = format!("({})", params);
                }
                write!(f, "{} -> {:?}", params, body)
            }
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Self::Nat => write!(f, "Nat"),
            Self::Int => write!(f, "Int"),
            Self::Rational => write!(f, "Rational"),
            Self::Real => write!(f, "Real"),
            Self::Complex => write!(f, "Complex"),
            Self::String => write!(f, "String"),
            Self::Char => write!(f, "Char"),
            Self::Bool => write!(f, "Bool"),
            Self::Var(n) => write!(f, "{}", n),
            Self::Lambda { params, body } => {
                let mut params = params
                    .into_iter()
                    .map(|param| format!("{}", param))
                    .collect::<Vec<_>>()
                    .join(", ");
                if params.len() > 1 {
                    params = format!("({})", params);
                }
                write!(f, "{} -> {}", params, body)
            }
        }
    }
}

type Substitution = HashMap<TyVar, Type>;

#[derive(Debug, Clone, PartialEq)]
struct Scheme {
    vars: Vec<TyVar>,
    ty: Type,
}
impl Scheme {
    fn new(vars: Vec<TyVar>, ty: Type) -> Self {
        Self { vars, ty }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyVar(pub usize);

static COUNTER: AtomicUsize = AtomicUsize::new(0);

impl TyVar {
    fn fresh() -> Self {
        Self(COUNTER.fetch_add(1, Ordering::SeqCst))
    }
}

impl Debug for TyVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "TyVar({})", self)
    }
}

#[rustfmt::skip]
const ALPHABET: &[char] = &[
    'a', 'b', 'c', 'd', 'e', 'f',
    'g', 'h', 'i', 'j', 'k', 'l',
    'm', 'n', 'o', 'p', 'q', 'r',
    's', 't', 'u', 'v', 'w', 'x',
    'y', 'z',
];

impl Display for TyVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 < ALPHABET.len() {
            write!(f, "'{}", ALPHABET[self.0])
        } else {
            write!(
                f,
                "'{}{}",
                ALPHABET[self.0 / ALPHABET.len() - 1],
                (self.0 + 1) % ALPHABET.len()
            )
        }
    }
}

fn apply_subst(subst: Substitution, ty: Type) -> Type {
    match ty {
        Type::Var(n) => subst.get(&n).cloned().unwrap_or(ty.clone()),
        Type::Lambda { params, body } => Type::Lambda {
            params: params
                .into_iter()
                .map(|param| apply_subst(subst.clone(), param))
                .collect(),
            body: Box::new(apply_subst(subst.clone(), *body)),
        },
        t => t.clone(),
    }
}

fn apply_subst_scheme(mut subst: Substitution, scheme: Scheme) -> Scheme {
    for var in &scheme.vars {
        subst.remove(var);
    }
    Scheme {
        vars: scheme.vars.clone(),
        ty: apply_subst(subst.clone(), scheme.ty.clone()),
    }
}

// left-biased union
fn map_union(m1: Substitution, m2: Substitution) -> Substitution {
    m2.into_iter().chain(m1.into_iter()).collect()
}

fn compose_subst(s1: Substitution, s2: Substitution) -> Substitution {
    map_union(
        s2.iter()
            .map(|(var, ty)| (var.clone(), apply_subst(s1.clone(), ty.clone())))
            .collect(),
        s1,
    )
}

fn free_vars(ty: Type) -> BTreeSet<TyVar> {
    match ty {
        Type::Var(n) => vec![n.clone()].into_iter().collect(),
        Type::Lambda { params, body } => params
            .into_iter()
            .map(|param| free_vars(param))
            .fold(free_vars(*body), |acc, set| {
                acc.union(&set).cloned().collect()
            }),
        _ => BTreeSet::new(),
    }
}

fn free_vars_scheme(scheme: Scheme) -> BTreeSet<TyVar> {
    free_vars(scheme.ty.clone())
        .difference(&scheme.vars.iter().cloned().collect())
        .cloned()
        .collect()
}

fn var_bind(var: TyVar, ty: Type) -> Result<Substitution, String> {
    if ty.clone() == Type::Var(var.clone()) {
        Ok(HashMap::new())
    } else if free_vars(ty.clone()).contains(&var) {
        Err(format!("occurs check failed: {} occurs in {:?}", var, ty))
    } else {
        let mut subst = HashMap::new();
        subst.insert(var, ty.clone());
        Ok(subst)
    }
}

pub fn unify(t1: Type, t2: Type) -> Result<Substitution, String> {
    match (t1.clone(), t2.clone()) {
        (Type::Int, Type::Int) => Ok(HashMap::new()),
        (Type::Bool, Type::Bool) => Ok(HashMap::new()),
        (
            Type::Lambda {
                params: p1,
                body: b1,
            },
            Type::Lambda {
                params: p2,
                body: b2,
            },
        ) => {
            let s1 = p1
                .iter()
                .zip(p2.iter())
                .try_fold(HashMap::new(), |acc, (ty1, ty2)| {
                    match unify(apply_subst(acc.clone(), ty1.clone()), ty2.clone()) {
                        Ok(s) => Ok(compose_subst(s, acc)),
                        Err(e) => Err(e),
                    }
                })?;
            let s2 = unify(
                apply_subst(s1.clone(), *b1.clone()),
                apply_subst(s1.clone(), *b2.clone()),
            )?;
            Ok(compose_subst(s1.clone(), s2.clone()))
        }
        (Type::Var(n), t) | (t, Type::Var(n)) => var_bind(n.clone(), t.clone()),
        _ => Err(format!(
            "cannot unify {:?} and {:?}",
            t1.lower(&mut HashMap::new()),
            t2.lower(&mut HashMap::new())
        )),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    vars: HashMap<InternedString, Scheme>,
}

fn apply_subst_ctx(subst: Substitution, ctx: Context) -> Context {
    Context {
        vars: ctx
            .vars
            .into_iter()
            .map(|(name, scheme)| (name, apply_subst_scheme(subst.clone(), scheme)))
            .collect(),
    }
}

fn free_vars_ctx(ctx: Context) -> BTreeSet<TyVar> {
    ctx.vars
        .into_iter()
        .map(|(_, scheme)| free_vars_scheme(scheme))
        .fold(BTreeSet::new(), |acc, set| {
            acc.union(&set).cloned().collect()
        })
}

fn generalize(ctx: Context, ty: Type) -> Scheme {
    Scheme {
        vars: free_vars(ty.clone())
            .difference(&free_vars_ctx(ctx))
            .cloned()
            .collect(),
        ty,
    }
}

fn instantiate(scheme: Scheme) -> Type {
    let mut subst = HashMap::new();
    for var in &scheme.vars {
        subst.insert(*var, Type::Var(TyVar::fresh()));
    }
    apply_subst(subst, scheme.ty)
}

fn infer(ctx: Context, expr: Expr) -> Result<(Substitution, Type), String> {
    match expr.clone() {
        // Expr::Int(_) => Ok((HashMap::new(), Type::Int)),
        // Expr::Bool(_) => Ok((HashMap::new(), Type::Bool)),
        Expr::Ident(name) => match ctx.clone().vars.get(name.inner()) {
            Some(scheme) => Ok((HashMap::new(), instantiate(scheme.clone()))),
            None => Err(format!("unbound variable: {:?}", expr)),
        },
        // Expr::Lambda(name, body) => {
        //     let ty_binder = Type::Var(TyVar::fresh());
        //     let tmp_ctx = Context {
        //         vars: ctx
        //             .clone()
        //             .vars
        //             .into_iter()
        //             .chain(vec![(name, Scheme::new(vec![], ty_binder.clone()))].into_iter())
        //             .collect(),
        //     };
        //     let (s1, t1) = infer(tmp_ctx, *body.clone())?;
        //     Ok((
        //         s1.clone(),
        //         Type::Lambda(
        //             Box::new(apply_subst(s1.clone(), ty_binder.clone())),
        //             Box::new(t1),
        //         ),
        //     ))
        // }
        Expr::Apply { fun, args } => {
            let ty_ret = Type::Var(TyVar::fresh());
            let (s1, ty_fun) = infer(ctx.clone(), fun.inner().clone())?;

            // let (s2, ty_arg) = infer(
            //     apply_subst_ctx(s1.clone(), ctx.clone()),
            //     arg.inner().clone(),
            // )?;

            let mut s = s1;
            let mut ty_args = Vec::new();

            for arg in args {
                let (s2, ty_arg) =
                    infer(apply_subst_ctx(s.clone(), ctx.clone()), arg.inner().clone())?;
                s = compose_subst(s2.clone(), s.clone());
                ty_args.push(ty_arg);
            }

            // let ty_fun_expected = ty_args.iter().fold(ty_ret.clone(), |acc, ty_arg| {
            //     // Type::Lambda(Box::new(ty_arg.clone()), Box::new(acc))
            //     Type::Lambda { params: , body: () }
            // });
            let ty_fun_expected = Type::Lambda {
                params: ty_args,
                body: Box::new(ty_ret.clone()),
            };

            let s3 = unify(apply_subst(s.clone(), ty_fun.clone()), ty_fun_expected)?;
            let sf = compose_subst(s3.clone(), s);

            Ok((sf, apply_subst(s3, ty_ret.clone())))
            // let s3 = unify(
            //     apply_subst(s2.clone(), ty_fun.clone()),
            //     Type::Lambda(Box::new(ty_arg.clone()), Box::new(ty_ret.clone())),
            // )?;
            // let sf = compose_subst(s3.clone(), compose_subst(s2.clone(), s1.clone()));
            // Ok((sf, apply_subst(s3, ty_ret.clone())))
        } // Expr::Let { name, expr, body } => {
        //     let (s1, t1) = infer(ctx.clone(), *expr.clone())?;
        //     let scheme = Scheme {
        //         vars: vec![],
        //         ty: apply_subst(s1.clone(), t1.clone()),
        //     };
        //     let tmp_ctx = Context {
        //         vars: ctx
        //             .vars
        //             .clone()
        //             .into_iter()
        //             .chain(vec![(name.clone(), scheme)].into_iter())
        //             .collect(),
        //     };
        //     let (s2, t2) = infer(apply_subst_ctx(s1.clone(), tmp_ctx), *body.clone())?;
        //     Ok((compose_subst(s2.clone(), s1.clone()), t2.clone()))
        // }
        Expr::Infix { op, lhs, rhs } => match op.inner() {
            InfixOp::Add => {
                // use table `allowed_types` of allowed type combinations
                todo!()
            }
            _ => todo!(),
        },
        // Expr::Add(l, r) => {
        //     let (s1, t1) = infer(ctx.clone(), *l.clone())?;
        //     let (s2, t2) = infer(apply_subst_ctx(s1.clone(), ctx.clone()), *r.clone())?;
        //     let s3 = unify(t1, Type::Int)?;
        //     let s4 = unify(t2, Type::Int)?;
        //     let sf = compose_subst(compose_subst(s4.clone(), s3.clone()), s2.clone());
        //     Ok((sf, Type::Int))
        // }
        // Expr::Sub(l, r) => {
        //     let (s1, t1) = infer(ctx.clone(), *l.clone())?;
        //     let (s2, t2) = infer(apply_subst_ctx(s1.clone(), ctx.clone()), *r.clone())?;
        //     let s3 = unify(t1, Type::Int)?;
        //     let s4 = unify(t2, Type::Int)?;
        //     Ok((
        //         compose_subst(compose_subst(s4.clone(), s3.clone()), s2.clone()),
        //         Type::Int,
        //     ))
        // }
        // Expr::Mul(l, r) => {
        //     let (s1, t1) = infer(ctx.clone(), *l.clone())?;
        //     let (s2, t2) = infer(apply_subst_ctx(s1.clone(), ctx.clone()), *r.clone())?;
        //     let s3 = unify(t1, Type::Int)?;
        //     let s4 = unify(t2, Type::Int)?;
        //     Ok((
        //         compose_subst(compose_subst(s4.clone(), s3.clone()), s2.clone()),
        //         Type::Int,
        //     ))
        // }
        // Expr::Div(l, r) => {
        //     let (s1, t1) = infer(ctx.clone(), *l.clone())?;
        //     let (s2, t2) = infer(apply_subst_ctx(s1.clone(), ctx.clone()), *r.clone())?;
        //     let s3 = unify(t1, Type::Int)?;
        //     let s4 = unify(t2, Type::Int)?;
        //     Ok((
        //         compose_subst(compose_subst(s4.clone(), s3.clone()), s2.clone()),
        //         Type::Int,
        //     ))
        // }
        _ => todo!(),
    }
}

pub fn type_inference(ctx: Context, expr: Expr) -> Result<Type, String> {
    let (subst, ty) = infer(ctx, expr)?;
    Ok(apply_subst(subst, ty).lower(&mut HashMap::new()))
}

pub fn default_ctx() -> Context {
    let mut ctx = Context {
        vars: HashMap::new(),
    };
    ctx.vars.insert(InternedString::from("id"), {
        let a = TyVar::fresh();
        Scheme::new(
            vec![a.clone()],
            Type::Lambda {
                params: vec![Type::Var(a.clone())],
                body: Box::new(Type::Var(a.clone())),
            },
        )
    });
    ctx.vars.insert(InternedString::from("const"), {
        let a = TyVar::fresh();
        let b = TyVar::fresh();
        Scheme::new(
            vec![a.clone(), b.clone()],
            Type::Lambda {
                params: vec![Type::Var(a.clone()), Type::Var(b.clone())],
                body: Box::new(Type::Var(a.clone())),
            },
        )
    });
    ctx
}
