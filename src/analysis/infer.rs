use super::res;
use crate::util::{intern::InternedString, node::SrcNode, unique_id::UniqueId};
use num_rational::Rational64;
use std::{
    collections::{BTreeSet, HashMap},
    fmt::{Debug, Display},
};

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct TypeError {
    pub msg: InternedString,
}

impl TypeError {
    pub fn new(msg: &str) -> Self {
        Self {
            msg: InternedString::from(msg),
        }
    }
}

impl From<String> for TypeError {
    fn from(msg: String) -> Self {
        Self::new(&*msg)
    }
}

pub type InferResult<T> = Result<T, TypeError>;

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    pub items: Vec<SrcNode<Item>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Expr(Expr),
    Def {
        name: SrcNode<UniqueId>,
        expr: SrcNode<Expr>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Lit {
        lit: Lit,
        ty: Type,
    },
    Ident {
        name: SrcNode<UniqueId>,
        ty: Type,
    },
    Lambda {
        params: Vec<SrcNode<UniqueId>>,
        body: SrcNode<Expr>,
        ty: Type,
    },
    Apply {
        fun: SrcNode<Expr>,
        args: Vec<SrcNode<Expr>>,
        ty: Type,
    },
    Let {
        name: SrcNode<UniqueId>,
        expr: SrcNode<Expr>,
        body: SrcNode<Expr>,
        ty: Type,
    },
    If {
        cond: SrcNode<Expr>,
        then: SrcNode<Expr>,
        else_: SrcNode<Expr>,
        ty: Type,
    },
    Infix {
        op: SrcNode<InfixOp>,
        lhs: SrcNode<Expr>,
        rhs: SrcNode<Expr>,
        ty: Type,
    },
    Prefix {
        op: SrcNode<PrefixOp>,
        expr: SrcNode<Expr>,
        ty: Type,
    },
    Unit,
}

impl Expr {
    pub fn ty(&self) -> Type {
        match self {
            Self::Lit { ty, .. } => ty.clone(),
            Self::Ident { ty, .. } => ty.clone(),
            Self::Lambda { ty, .. } => ty.clone(),
            Self::Apply { ty, .. } => ty.clone(),
            Self::Let { ty, .. } => ty.clone(),
            Self::If { ty, .. } => ty.clone(),
            Self::Infix { ty, .. } => ty.clone(),
            Self::Prefix { ty, .. } => ty.clone(),
            Self::Unit => Type::Unit,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrefixOp {
    Neg,
    Not,
}

impl ToString for PrefixOp {
    fn to_string(&self) -> String {
        match self {
            Self::Neg => "-",
            Self::Not => "!",
        }
        .to_string()
    }
}

impl From<res::PrefixOp> for PrefixOp {
    fn from(op: res::PrefixOp) -> Self {
        match op {
            res::PrefixOp::Neg => Self::Neg,
            res::PrefixOp::Not => Self::Not,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Leq,
    Geq,
}

impl ToString for InfixOp {
    fn to_string(&self) -> String {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Mod => "%",
            Self::Eq => "==",
            Self::Neq => "!=",
            Self::Lt => "<",
            Self::Gt => ">",
            Self::Leq => "<=",
            Self::Geq => ">=",
        }
        .to_string()
    }
}

impl From<res::InfixOp> for InfixOp {
    fn from(op: res::InfixOp) -> Self {
        match op {
            res::InfixOp::Add => Self::Add,
            res::InfixOp::Sub => Self::Sub,
            res::InfixOp::Mul => Self::Mul,
            res::InfixOp::Div => Self::Div,
            res::InfixOp::Mod => Self::Mod,
            res::InfixOp::Eq => Self::Eq,
            res::InfixOp::Neq => Self::Neq,
            res::InfixOp::Lt => Self::Lt,
            res::InfixOp::Gt => Self::Gt,
            res::InfixOp::Leq => Self::Leq,
            res::InfixOp::Geq => Self::Geq,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Num(Rational64),
    Bool(bool),
}

#[derive(Clone, PartialEq)]
pub enum Type {
    Num,
    Bool,
    Var(TyVar),
    Lambda(Vec<Self>, Box<Self>),
    Unit,
}

impl Type {
    fn lower(&self, vars: &mut HashMap<TyVar, TyVar>) -> Self {
        match self.clone() {
            Self::Num => Self::Num,
            Self::Bool => Self::Bool,
            Self::Var(name) => {
                if let Some(n) = vars.get(&name) {
                    Self::Var(*n)
                } else {
                    let n = vars.len();
                    vars.insert(name, TyVar(n.into()));
                    Self::Var(TyVar(n.into()))
                }
            }
            Self::Lambda(params, body) => {
                let mut lowered_params = vec![];
                for param in params {
                    lowered_params.push(param.lower(vars));
                }
                Self::Lambda(lowered_params, Box::new(body.lower(vars)))
            }
            Self::Unit => Self::Unit,
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Self::Num => write!(f, "Num"),
            Self::Bool => write!(f, "Bool"),
            Self::Var(n) => write!(f, "{:?}", n),
            Self::Lambda(params, body) => write!(f, "{:?} -> {:?}", params, body),
            Self::Unit => write!(f, "()"),
        }
    }
}

// impl Display for Type {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self.clone() {
//             Self::Num => write!(f, "Int"),
//             Self::Bool => write!(f, "Bool"),
//             Self::Var(n) => write!(f, "{}", n),
//             Self::Lambda(params, body) => write!(f, "({}) -> {}", params, body),
//         }
//     }
// }

// type Substitution = HashMap<TyVar, Type>;

#[derive(Debug, Clone, PartialEq)]
struct Substitution {
    map: HashMap<TyVar, Type>,
}

impl Substitution {
    fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    fn get(&self, var: &TyVar) -> Option<&Type> {
        self.map.get(var)
    }

    fn insert(&mut self, var: TyVar, ty: Type) {
        self.map.insert(var, ty);
    }

    fn remove(&mut self, var: &TyVar) -> Option<Type> {
        self.map.remove(var)
    }

    // left-biased union
    fn union(&self, other: Self) -> Self {
        self.clone()
            .into_iter()
            .chain(other.clone().into_iter())
            .collect()
    }

    fn compose(&self, other: Substitution) -> Self {
        self.union(
            other
                .into_iter()
                .map(|(var, ty)| (var, apply_subst(self.clone(), ty)))
                .collect(),
        )
    }
}

impl FromIterator<(TyVar, Type)> for Substitution {
    fn from_iter<T: IntoIterator<Item = (TyVar, Type)>>(iter: T) -> Self {
        Self {
            map: iter.into_iter().collect(),
        }
    }
}

impl IntoIterator for Substitution {
    type Item = (TyVar, Type);
    type IntoIter = std::collections::hash_map::IntoIter<TyVar, Type>;

    fn into_iter(self) -> Self::IntoIter {
        self.map.into_iter()
    }
}

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
pub struct TyVar(UniqueId);

impl TyVar {
    fn fresh() -> Self {
        Self(UniqueId::gen())
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
        let id = usize::from(self.0);
        if id < ALPHABET.len() {
            write!(f, "'{}", ALPHABET[id])
        } else {
            write!(
                f,
                "'{}{}",
                ALPHABET[id / ALPHABET.len() - 1],
                (id + 1) % ALPHABET.len()
            )
        }
    }
}

fn apply_subst(subst: Substitution, ty: Type) -> Type {
    match ty {
        Type::Num | Type::Bool | Type::Unit => ty.clone(),
        Type::Var(n) => subst.get(&n).cloned().unwrap_or(ty.clone()),
        Type::Lambda(params, body) => Type::Lambda(
            params
                .into_iter()
                .map(|p| apply_subst(subst.clone(), p))
                .collect(),
            Box::new(apply_subst(subst.clone(), *body)),
        ),
    }
}

fn apply_subst_vec(subst: Substitution, tys: Vec<Type>) -> Vec<Type> {
    tys.into_iter()
        .map(|ty| apply_subst(subst.clone(), ty))
        .collect()
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

fn free_vars(ty: Type) -> BTreeSet<TyVar> {
    match ty {
        Type::Var(n) => vec![n.clone()].into_iter().collect(),
        Type::Lambda(params, body) => params
            .into_iter()
            .fold(BTreeSet::new(), |acc, p| {
                free_vars(p).union(&acc).cloned().collect()
            })
            .union(&free_vars(*body.clone()))
            .cloned()
            .collect(),
        _ => BTreeSet::new(),
    }
}

fn free_vars_scheme(scheme: Scheme) -> BTreeSet<TyVar> {
    free_vars(scheme.ty.clone())
        .difference(&scheme.vars.iter().cloned().collect())
        .cloned()
        .collect()
}

fn var_bind(var: TyVar, ty: Type) -> InferResult<Substitution> {
    if ty.clone() == Type::Var(var.clone()) {
        Ok(Substitution::new())
    } else if free_vars(ty.clone()).contains(&var) {
        Err(TypeError::from(format!(
            "occurs check failed: {} occurs in {:?}",
            var, ty
        )))
    } else {
        let mut subst = Substitution::new();
        subst.insert(var, ty.clone());
        Ok(subst)
    }
}

fn unify(t1: Type, t2: Type) -> InferResult<Substitution> {
    // println!("unify: {:?} and {:?}", t1, t2);
    match (t1.clone(), t2.clone()) {
        (Type::Num, Type::Num) => Ok(Substitution::new()),
        (Type::Bool, Type::Bool) => Ok(Substitution::new()),
        (Type::Lambda(p1, b1), Type::Lambda(p2, b2)) => {
            let s1 = p1.into_iter().zip(p2.into_iter()).fold(
                Ok(Substitution::new()),
                |acc: InferResult<Substitution>, (t1, t2)| {
                    let s = acc?;
                    let s1 = unify(t1, t2)?;
                    Ok(s.compose(s1))
                },
            )?;
            // println!("unify s1: {:?}", s1);
            let s2 = unify(
                apply_subst(s1.clone(), *b1.clone()),
                apply_subst(s1.clone(), *b2.clone()),
            )?;
            // println!("unify s2: {:?}", s2);
            // println!("unify s1.compose(s2): {:?}", s1.compose(s2.clone()));
            Ok(s1.compose(s2.clone()))
        }
        (Type::Var(n), t) | (t, Type::Var(n)) => var_bind(n.clone(), t.clone()),
        _ => Err(TypeError::from(format!(
            "cannot unify {:?} and {:?}",
            t1.lower(&mut HashMap::new()),
            t2.lower(&mut HashMap::new())
        ))),
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Context {
    vars: HashMap<UniqueId, Scheme>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
        }
    }

    fn extend(&mut self, id: UniqueId, scheme: Scheme) {
        self.vars.insert(id, scheme);
    }

    fn union(&self, other: Self) -> Self {
        Self {
            vars: self
                .vars
                .clone()
                .into_iter()
                .chain(other.vars.clone().into_iter())
                .collect(),
        }
    }

    fn apply_subst(&self, subst: Substitution) -> Self {
        Self {
            vars: self
                .vars
                .clone()
                .into_iter()
                .map(|(id, scheme)| (id, apply_subst_scheme(subst.clone(), scheme)))
                .collect(),
        }
    }
}

// fn apply_subst_ctx(subst: Substitution, ctx: Context) -> Context {
//     Context {
//         vars: ctx
//             .vars
//             .into_iter()
//             .map(|(name, scheme)| (name, apply_subst_scheme(subst.clone(), scheme)))
//             .collect(),
//     }
// }

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
    let mut subst = Substitution::new();
    for var in &scheme.vars {
        subst.insert(*var, Type::Var(TyVar::fresh()));
    }
    apply_subst(subst, scheme.ty)
}

fn infer_item(
    ctx: &mut Context,
    item: SrcNode<res::Item>,
) -> InferResult<(Substitution, Type, Context, SrcNode<Item>)> {
    match item.inner().clone() {
        res::Item::Expr(expr) => {
            let (s, t, e) = infer_expr(ctx, SrcNode::new(expr, item.span()))?;
            Ok((
                s,
                t,
                ctx.clone(),
                SrcNode::new(Item::Expr(e.inner().clone()), item.span()),
            ))
        }
        res::Item::Def { name, expr } => {
            // println!("infer_item: {:?}", item);
            // println!("ctx: {:?}", ctx);
            let mut e_ctx = ctx.clone();
            let f_var = Type::Var(TyVar::fresh());
            e_ctx.extend(*name, Scheme::new(vec![], f_var.clone()));
            // println!("e_ctx: {:?}", e_ctx);
            let (s1, t1, expr) = infer_expr(&mut e_ctx, expr)?;
            let s2 = unify(f_var, t1.clone())?;
            // println!("s1: {:?}", s1);
            // println!("s2: {:?}", s2);
            let scheme = generalize(ctx.clone(), t1.clone());
            // println!("scheme: {:?}", scheme);
            let mut tmp_ctx = ctx.clone();
            tmp_ctx.extend(*name, scheme);
            // println!("tmp_ctx: {:?}", tmp_ctx);
            Ok((
                s1.compose(s2),
                t1,
                ctx.union(tmp_ctx).union(e_ctx),
                SrcNode::new(Item::Def { name, expr }, item.span()),
            ))
        }
    }
}

fn infer_expr(
    ctx: &mut Context,
    expr: SrcNode<res::Expr>,
) -> InferResult<(Substitution, Type, SrcNode<Expr>)> {
    match expr.inner().clone() {
        res::Expr::Lit(l) => match l.inner() {
            res::Lit::Num(n) => Ok((
                Substitution::new(),
                Type::Num,
                SrcNode::new(
                    Expr::Lit {
                        lit: Lit::Num(n.clone()),
                        ty: Type::Num,
                    },
                    expr.span(),
                ),
            )),
            res::Lit::Bool(b) => Ok((
                Substitution::new(),
                Type::Bool,
                SrcNode::new(
                    Expr::Lit {
                        lit: Lit::Bool(b.clone()),
                        ty: Type::Bool,
                    },
                    expr.span(),
                ),
            )),
        },
        res::Expr::Ident(name) => match ctx.clone().vars.get(&name) {
            Some(scheme) => {
                let ty = instantiate(scheme.clone());
                Ok((
                    Substitution::new(),
                    ty.clone(),
                    SrcNode::new(
                        Expr::Ident {
                            name: name.clone(),
                            ty: ty.clone(),
                        },
                        expr.span(),
                    ),
                ))
            }
            None => Err(TypeError::from(format!("unbound variable: {:?}", expr))),
        },
        res::Expr::Lambda { params, body } => {
            let mut ty_binders = vec![];
            let mut tmp_ctx = ctx.clone();
            // println!("lam ctx: {:?}", ctx);
            for p in params.clone() {
                let ty_binder = Type::Var(TyVar::fresh());
                ty_binders.push(ty_binder.clone());
                tmp_ctx.extend(*p, Scheme::new(vec![], ty_binder));
            }
            // println!("lam tmp_ctx: {:?}", tmp_ctx);
            // println!("lam ty_binders: {:?}", ty_binders);
            let (s1, t1, e1) = infer_expr(&mut tmp_ctx, body)?;
            let ty = Type::Lambda(
                apply_subst_vec(s1.clone(), ty_binders.clone()),
                Box::new(t1),
            );
            Ok((
                s1.clone(),
                ty.clone(),
                SrcNode::new(
                    Expr::Lambda {
                        params,
                        body: e1,
                        ty,
                    },
                    expr.span(),
                ),
            ))
        }
        res::Expr::Apply { fun, args } => {
            let ty_ret = Type::Var(TyVar::fresh());
            let (s1, ty_fun, e1) = infer_expr(&mut ctx.clone(), fun.clone())?;
            let s2 = Substitution::new();
            let mut ty_args = vec![];
            let mut targs = vec![];
            for arg in args {
                let (s_arg, ty_arg, ea) = infer_expr(&mut ctx.apply_subst(s1.clone()), arg)?;
                targs.push(ea.clone());
                s2.compose(s_arg);
                ty_args.push(ty_arg);
            }
            let s3 = unify(
                apply_subst(s2.clone(), ty_fun.clone()),
                Type::Lambda(ty_args.clone(), Box::new(ty_ret.clone())),
            )?;
            let sf = s3.compose(s2.compose(s1.clone()));
            let ty = apply_subst(s3, ty_ret.clone());
            Ok((
                sf,
                ty.clone(),
                SrcNode::new(
                    Expr::Apply {
                        fun: e1,
                        args: targs,
                        ty,
                    },
                    expr.span(),
                ),
            ))
        }
        res::Expr::Let {
            name,
            expr: binding,
            body,
        } => {
            let mut e_ctx = ctx.clone();
            let f_var = Type::Var(TyVar::fresh());
            e_ctx.extend(*name, Scheme::new(vec![], f_var.clone()));
            let (s1, t1, e1) = infer_expr(&mut e_ctx.clone(), binding.clone())?;
            let s2 = unify(f_var, t1.clone())?;
            let scheme = generalize(ctx.clone(), t1.clone());
            let mut tmp_ctx = ctx.clone();
            tmp_ctx.extend(*name, scheme);
            let (s3, t2, e2) = infer_expr(&mut tmp_ctx, body.clone())?;
            Ok((
                s3.compose(s2).compose(s1),
                t2.clone(),
                SrcNode::new(
                    Expr::Let {
                        name,
                        expr: e1,
                        body: e2,
                        ty: t2,
                    },
                    expr.span(),
                ),
            ))
        }
        res::Expr::If { cond, then, else_ } => {
            let (s1, t1, e1) = infer_expr(&mut ctx.clone(), cond.clone())?;
            let s2 = unify(t1, Type::Bool)?;
            let (s3, t2, e2) = infer_expr(&mut ctx.apply_subst(s1.clone()), then.clone())?;
            let (s4, t3, e3) = infer_expr(&mut ctx.apply_subst(s3.clone()), else_.clone())?;
            let s5 = unify(t2.clone(), t3)?;
            let sf = s5.compose(s4.compose(s3.compose(s2.compose(s1.clone()))));
            Ok((
                sf,
                t2.clone(),
                SrcNode::new(
                    Expr::If {
                        cond: e1,
                        then: e2,
                        else_: e3,
                        ty: t2,
                    },
                    expr.span(),
                ),
            ))
        }
        res::Expr::Infix { op, lhs, rhs } => match *op {
            res::InfixOp::Add
            | res::InfixOp::Sub
            | res::InfixOp::Mul
            | res::InfixOp::Div
            | res::InfixOp::Mod => {
                let (s1, t1, e1) = infer_expr(&mut ctx.clone(), lhs.clone())?;
                let (s2, t2, e2) = infer_expr(&mut ctx.apply_subst(s1.clone()), rhs.clone())?;
                let s3 = unify(t1, Type::Num)?;
                let s4 = unify(t2, Type::Num)?;
                let sf = s4.compose(s3.compose(s2.clone()));
                Ok((
                    sf,
                    Type::Num,
                    SrcNode::new(
                        Expr::Infix {
                            op: SrcNode::new(InfixOp::from(op.inner().clone()), op.span()),
                            lhs: e1,
                            rhs: e2,
                            ty: Type::Num,
                        },
                        expr.span(),
                    ),
                ))
            }
            res::InfixOp::Eq | res::InfixOp::Neq => {
                let (s1, t1, e1) = infer_expr(&mut ctx.clone(), lhs.clone())?;
                let (s2, t2, e2) = infer_expr(&mut ctx.apply_subst(s1.clone()), rhs.clone())?;
                let s3 = unify(t1, t2)?;
                let sf = s3.compose(s2.compose(s1.clone()));
                Ok((
                    sf,
                    Type::Bool,
                    SrcNode::new(
                        Expr::Infix {
                            op: SrcNode::new(InfixOp::from(op.inner().clone()), op.span()),
                            lhs: e1,
                            rhs: e2,
                            ty: Type::Bool,
                        },
                        expr.span(),
                    ),
                ))
            }
            res::InfixOp::Lt | res::InfixOp::Gt | res::InfixOp::Leq | res::InfixOp::Geq => {
                let (s1, t1, e1) = infer_expr(&mut ctx.clone(), lhs.clone())?;
                let (s2, t2, e2) = infer_expr(&mut ctx.apply_subst(s1.clone()), rhs.clone())?;
                let s3 = unify(t1, Type::Num)?;
                let s4 = unify(t2, Type::Num)?;
                let sf = s4.compose(s3.compose(s2.clone()));
                Ok((
                    sf,
                    Type::Bool,
                    SrcNode::new(
                        Expr::Infix {
                            op: SrcNode::new(InfixOp::from(op.inner().clone()), op.span()),
                            lhs: e1,
                            rhs: e2,
                            ty: Type::Bool,
                        },
                        expr.span(),
                    ),
                ))
            }
        },
        res::Expr::Prefix { op, expr } => {
            let (s1, t1, e1) = infer_expr(&mut ctx.clone(), expr.clone())?;
            match op.inner() {
                res::PrefixOp::Neg => {
                    let s2 = unify(t1, Type::Num)?;
                    Ok((
                        s2.compose(s1),
                        Type::Num,
                        SrcNode::new(
                            Expr::Prefix {
                                op: SrcNode::new(PrefixOp::Neg, op.span()),
                                expr: e1,
                                ty: Type::Num,
                            },
                            expr.span(),
                        ),
                    ))
                }
                res::PrefixOp::Not => {
                    let s2 = unify(t1, Type::Bool)?;
                    Ok((
                        s2.compose(s1),
                        Type::Bool,
                        SrcNode::new(
                            Expr::Prefix {
                                op: SrcNode::new(PrefixOp::Not, op.span()),
                                expr: e1,
                                ty: Type::Bool,
                            },
                            expr.span(),
                        ),
                    ))
                }
            }
        }
        res::Expr::Unit => Ok((
            Substitution::new(),
            Type::Unit,
            SrcNode::new(Expr::Unit, expr.span()),
        )),
    }
}

fn apply_subst_root(subst: Substitution, root: Root) -> Root {
    Root {
        items: root
            .items
            .into_iter()
            .map(|item| apply_subst_item(subst.clone(), item))
            .collect(),
    }
}

fn apply_subst_item(subst: Substitution, item: SrcNode<Item>) -> SrcNode<Item> {
    SrcNode::new(
        match item.inner().clone() {
            Item::Expr(expr) => Item::Expr(
                apply_subst_expr(subst, SrcNode::new(expr, item.span()))
                    .inner()
                    .clone(),
            ),
            Item::Def { name, expr } => Item::Def {
                name,
                expr: apply_subst_expr(subst, expr),
            },
        },
        item.span(),
    )
}

fn apply_subst_expr(subst: Substitution, expr: SrcNode<Expr>) -> SrcNode<Expr> {
    SrcNode::new(
        match expr.inner().clone() {
            Expr::Lit { lit, ty } => Expr::Lit {
                lit,
                ty: apply_subst(subst, ty),
            },
            Expr::Ident { name, ty } => Expr::Ident {
                name,
                ty: apply_subst(subst, ty),
            },
            Expr::Lambda { params, body, ty } => Expr::Lambda {
                params,
                body: apply_subst_expr(subst.clone(), body),
                ty: apply_subst(subst, ty),
            },
            Expr::Apply { fun, args, ty } => Expr::Apply {
                fun: apply_subst_expr(subst.clone(), fun),
                args: args
                    .into_iter()
                    .map(|arg| apply_subst_expr(subst.clone(), arg))
                    .collect(),
                ty: apply_subst(subst, ty),
            },
            Expr::Let {
                name,
                expr,
                body,
                ty,
            } => Expr::Let {
                name,
                expr: apply_subst_expr(subst.clone(), expr),
                body: apply_subst_expr(subst.clone(), body),
                ty: apply_subst(subst, ty),
            },
            Expr::If {
                cond,
                then,
                else_,
                ty,
            } => Expr::If {
                cond: apply_subst_expr(subst.clone(), cond),
                then: apply_subst_expr(subst.clone(), then),
                else_: apply_subst_expr(subst.clone(), else_),
                ty: apply_subst(subst, ty),
            },
            Expr::Infix { op, lhs, rhs, ty } => Expr::Infix {
                op,
                lhs: apply_subst_expr(subst.clone(), lhs),
                rhs: apply_subst_expr(subst.clone(), rhs),
                ty: apply_subst(subst, ty),
            },
            Expr::Prefix { op, expr, ty } => Expr::Prefix {
                op,
                expr: apply_subst_expr(subst.clone(), expr),
                ty: apply_subst(subst, ty),
            },
            Expr::Unit => Expr::Unit,
        },
        expr.span(),
    )
}

pub fn type_inference(
    ctx: &mut Context,
    root: SrcNode<res::Root>,
) -> InferResult<(SrcNode<Root>, Context)> {
    let mut s = Substitution::new();
    let mut items = vec![];
    let mut ctx_ret = Context::new();
    for item in root.inner().items.clone() {
        let (subst, _, c, i) = infer_item(ctx, item)?;
        // types.push(apply_subst(subst, ty).lower(&mut HashMap::new()));
        ctx_ret = ctx_ret.union(c);
        items.push(i);
        s = subst.compose(s);
    }
    println!("s: {:?}", s);
    Ok((
        // SrcNode::new(apply_subst_root(s, Root { items }), root.span().clone()),
        SrcNode::new(Root { items }, root.span().clone()),
        ctx_ret,
    ))
}

// mod tests {
//     use super::res::resolve;
//     use crate::{analysis::res, syntax::parse::parse, util::node::SrcNode};

//     #[test]
//     fn infer_num() {
//         let (ast, errors) = parse("1");
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let (res, errors) = resolve(&ast.unwrap());
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }

//         let ctx = super::Context::new();
//         if let Ok(types) = super::type_inference(ctx, res.unwrap()) {
//             insta::assert_debug_snapshot!(types);
//         } else {
//             panic!("inference failed: {:?}", errors);
//         }
//     }

//     #[test]
//     fn infer_bool() {
//         let (ast, errors) = parse("true");
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let (res, errors) = resolve(&ast.unwrap());
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }

//         let ctx = super::Context::new();
//         if let Ok(types) = super::type_inference(ctx, res.unwrap()) {
//             insta::assert_debug_snapshot!(types);
//         } else {
//             panic!("inference failed: {:?}", errors);
//         }
//     }

//     #[test]
//     fn infer_def() {
//         let (ast, errors) = parse("x = 1");
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let (res, errors) = resolve(&ast.unwrap());
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }

//         let ctx = super::Context::new();
//         if let Ok(types) = super::type_inference(ctx, res.unwrap()) {
//             insta::assert_debug_snapshot!(types);
//         } else {
//             panic!("inference failed: {:?}", errors);
//         }
//     }

//     #[test]
//     fn infer_prefix() {
//         let (ast, errors) = parse("-1");
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let (res, errors) = resolve(&ast.unwrap());
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }

//         let ctx = super::Context::new();
//         if let Ok(types) = super::type_inference(ctx, res.unwrap()) {
//             insta::assert_debug_snapshot!(types);
//         } else {
//             panic!("inference failed: {:?}", errors);
//         }
//     }

//     #[test]
//     fn infer_infix() {
//         let (ast, errors) = parse("1 + 2");
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let (res, errors) = resolve(&ast.unwrap());
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }

//         let ctx = super::Context::new();
//         if let Ok(types) = super::type_inference(ctx, res.unwrap()) {
//             insta::assert_debug_snapshot!(types);
//         } else {
//             panic!("inference failed: {:?}", errors);
//         }
//     }

//     #[test]
//     fn infer_fn_def() {
//         let (ast, errors) = parse("f x = x");
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let (res, errors) = resolve(&ast.unwrap());
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }

//         let ctx = super::Context::new();
//         if let Ok(types) = super::type_inference(ctx, res.unwrap()) {
//             insta::assert_debug_snapshot!(types);
//         } else {
//             panic!("inference failed: {:?}", errors);
//         }
//     }

//     #[test]
//     fn infer_infix_fn_def() {
//         let (ast, errors) = parse("add x y = x + y");
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let (res, errors) = resolve(&ast.unwrap());
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }

//         let ctx = super::Context::new();
//         if let Ok(types) = super::type_inference(ctx, res.unwrap()) {
//             insta::assert_debug_snapshot!(types);
//         } else {
//             panic!("inference failed: {:?}", errors);
//         }
//     }

//     #[test]
//     fn infer_apply_second_order() {
//         let (ast, errors) = parse("f g x = g x");
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let (res, errors) = resolve(&ast.unwrap());
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }

//         let ctx = super::Context::new();
//         if let Ok(types) = super::type_inference(ctx, res.unwrap()) {
//             insta::assert_debug_snapshot!(types);
//         } else {
//             panic!("inference failed: {:?}", errors);
//         }
//     }

//     #[test]
//     fn infer_apply_rec() {
//         let (ast, errors) = parse("f x = f x");
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let (res, errors) = resolve(&ast.unwrap());
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }

//         let ctx = super::Context::new();
//         match super::type_inference(ctx, res.unwrap()) {
//             Ok(types) => insta::assert_debug_snapshot!(types),
//             Err(errors) => panic!("inference failed: {:?}", errors),
//         }
//     }

//     #[test]
//     fn infer_let() {
//         let (ast, errors) = parse("let x = 1 in x");
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let (res, errors) = resolve(&ast.unwrap());
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let ctx = super::Context::new();
//         match super::type_inference(ctx, res.unwrap()) {
//             Ok(types) => insta::assert_debug_snapshot!(types),
//             Err(errors) => panic!("inference failed: {:?}", errors),
//         }
//     }

//     #[test]
//     fn infer_let_infix() {
//         let (ast, errors) = parse("let x = 1 in x + 1");
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let (res, errors) = resolve(&ast.unwrap());
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let ctx = super::Context::new();
//         match super::type_inference(ctx, res.unwrap()) {
//             Ok(types) => insta::assert_debug_snapshot!(types),
//             Err(errors) => panic!("inference failed: {:?}", errors),
//         }
//     }

//     #[test]
//     fn infer_let_fn() {
//         let (ast, errors) = parse("let id x = x in id id");
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let (res, errors) = resolve(&ast.unwrap());
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let ctx = super::Context::new();
//         match super::type_inference(ctx, res.unwrap()) {
//             Ok(types) => insta::assert_debug_snapshot!(types),
//             Err(errors) => panic!("inference failed: {:?}", errors),
//         }
//     }

//     #[test]
//     fn infer_let_rec() {
//         let (ast, errors) = parse("let f x = f x in f f");
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let (res, errors) = resolve(&ast.unwrap());
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let ctx = super::Context::new();
//         match super::type_inference(ctx, res.unwrap()) {
//             Ok(types) => insta::assert_debug_snapshot!(types),
//             Err(errors) => panic!("inference failed: {:?}", errors),
//         }
//     }

//     #[test]
//     fn infer_rec_loop() {
//         let (ast, errors) = parse("f x = f");
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let (res, errors) = resolve(&ast.unwrap());
//         if !errors.is_empty() {
//             panic!("{:?}", errors);
//         }
//         let ctx = super::Context::new();
//         match super::type_inference(ctx, res.unwrap()) {
//             Ok(types) => insta::assert_debug_snapshot!(types),
//             Err(errors) => panic!("inference failed: {:?}", errors),
//         }
//     }

//     // #[test]
//     // fn infer_infinite() {
//     //     let res = res::Root {
//     //         items: vec![SrcNode::new(res::Item::Def {
//     //             name: SrcNode::new(0.into(), 0..0),
//     //             expr: SrcNode::new(
//     //                 res::Expr::Apply {
//     //                     fun: Box::new(SrcNode::new(res::Expr::Ident(0.into()), 0..0)),
//     //                     args: vec![SrcNode::new(res::Expr::Ident(0.into()), 0..0)],
//     //                 },
//     //                 0..0,
//     //             ),
//     //         })],
//     //     };
//     // }
// }
