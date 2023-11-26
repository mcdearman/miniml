use super::res;
use crate::util::{intern::InternedString, node::Node, unique_id::UniqueId};
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
    pub items: Vec<Node<Item>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Expr(Expr),
    Def {
        pat: Node<Pattern>,
        expr: Node<Expr>,
        ty: Type,
    },
    Fn {
        name: Node<UniqueId>,
        params: Vec<Node<Pattern>>,
        body: Node<Expr>,
        ty: Type,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Lit {
        lit: Lit,
        ty: Type,
    },
    Ident {
        name: Node<UniqueId>,
        ty: Type,
    },
    Lambda {
        params: Vec<Node<Pattern>>,
        body: Node<Expr>,
        ty: Type,
    },
    Apply {
        fun: Node<Expr>,
        args: Vec<Node<Expr>>,
        ty: Type,
    },
    Let {
        pat: Node<Pattern>,
        expr: Node<Expr>,
        body: Node<Expr>,
        ty: Type,
    },
    Fn {
        name: Node<UniqueId>,
        params: Vec<Node<Pattern>>,
        expr: Node<Expr>,
        body: Node<Expr>,
        ty: Type,
    },
    Match {
        expr: Node<Expr>,
        cases: Vec<Node<MatchCase>>,
        ty: Type,
    },
    If {
        cond: Node<Expr>,
        then: Node<Expr>,
        else_: Node<Expr>,
        ty: Type,
    },
    Unit,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchCase {
    pub pattern: Node<Pattern>,
    pub expr: Node<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pattern {
    Lit(Lit),
    Ident(UniqueId),
    Wildcard,
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
            Self::Fn { ty, .. } => ty.clone(),
            Self::Match { ty, .. } => ty.clone(),
            Self::If { ty, .. } => ty.clone(),
            Self::Unit => Type::Unit,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Lit {
    Num(Rational64),
    Bool(bool),
    String(InternedString),
}

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    Num,
    Bool,
    String,
    Var(TyVar),
    Lambda(Vec<Self>, Box<Self>),
    Unit,
}

impl Type {
    fn lower(&self, vars: &mut HashMap<TyVar, TyVar>) -> Self {
        match self.clone() {
            Self::Num => Self::Num,
            Self::Bool => Self::Bool,
            Self::String => Self::String,
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
            Self::String => write!(f, "String"),
            Self::Var(n) => write!(f, "{:?}", n),
            Self::Lambda(params, body) => write!(f, "{:?} -> {:?}", params, body),
            Self::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
enum Constraint {
    Eq(Type, Type),
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
pub struct Scheme {
    vars: Vec<TyVar>,
    ty: Type,
}

impl Scheme {
    pub fn new(vars: Vec<TyVar>, ty: Type) -> Self {
        Self { vars, ty }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TyVar(UniqueId);

impl TyVar {
    pub fn fresh() -> Self {
        Self(UniqueId::gen())
    }
}

impl From<UniqueId> for TyVar {
    fn from(id: UniqueId) -> Self {
        Self(id)
    }
}

impl From<usize> for TyVar {
    fn from(id: usize) -> Self {
        Self(UniqueId::from(id))
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
        Type::Num | Type::Bool | Type::String | Type::Unit => ty.clone(),
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
        (Type::String, Type::String) => Ok(Substitution::new()),
        (Type::Lambda(p1, b1), Type::Lambda(p2, b2)) => {
            let s1 = p1.into_iter().zip(p2.into_iter()).fold(
                Ok(Substitution::new()),
                |acc: InferResult<Substitution>, (t1, t2)| {
                    let s = acc?;
                    let s1 = unify(apply_subst(s.clone(), t1), apply_subst(s.clone(), t2))?;
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

    pub fn extend(&mut self, id: UniqueId, scheme: Scheme) {
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

fn infer_item<'src>(
    src: &'src str,
    ctx: &mut Context,
    item: Node<res::Item>,
) -> InferResult<(Vec<Constraint>, Context, Node<Item>)> {
    match item.inner().clone() {
        res::Item::Def { pat, expr } => {
            let (cs, _, ectx, e) = infer_expr(src, ctx, expr)?;
            let scheme = generalize(ectx.clone(), e.inner().ty());
            let mut tmp_ctx = ectx.clone();
            let (cs, t, ctx, pat) = infer_pattern(src, &mut tmp_ctx, pat)?;
            Ok((
                cs,
                ctx.union(tmp_ctx),
                Node::new(
                    Item::Def {
                        pat,
                        expr: e.clone(),
                        ty: e.inner().ty(),
                    },
                    item.span(),
                ),
            ))
        }
        res::Item::Fn { name, params, body } => {
            let mut e_ctx = ctx.clone();
            let f_var = Type::Var(TyVar::fresh());
            e_ctx.extend(*name, Scheme::new(vec![], f_var.clone()));
            let mut ty_binders = vec![];
            let mut tmp_ctx = e_ctx.clone();
            let mut new_params = vec![];
            for p in params.clone() {
                let ty_binder = Type::Var(TyVar::fresh());
                ty_binders.push(ty_binder.clone());
                let (cs, ty, ctx, pat) = infer_pattern(src, &mut tmp_ctx, p)?;
                tmp_ctx = ctx.union(tmp_ctx);
                new_params.push(pat);
                // tmp_ctx.extend(*p, Scheme::new(vec![], ty_binder));
            }
            let (cs1, t1, c, body) = infer_expr(src, &mut tmp_ctx, body)?;
            let ty = Type::Lambda(ty_binders.clone(), Box::new(t1.clone()));
            tmp_ctx.extend(*name, Scheme::new(vec![], ty.clone()));

            Ok((
                cs1,
                tmp_ctx.union(c),
                Node::new(
                    Item::Fn {
                        name,
                        params: new_params,
                        body,
                        ty,
                    },
                    item.span(),
                ),
            ))
        }
        res::Item::Expr(expr) => {
            let (cs, _, ectx, e) = infer_expr(src, ctx, Node::new(expr, item.span()))?;
            Ok((
                cs,
                ectx,
                Node::new(Item::Expr(e.inner().clone()), item.span()),
            ))
        }
    }
}

fn infer_expr<'src>(
    src: &'src str,
    ctx: &mut Context,
    expr: Node<res::Expr>,
) -> InferResult<(Vec<Constraint>, Type, Context, Node<Expr>)> {
    match expr.inner().clone() {
        res::Expr::Lit(lit) => match *lit {
            res::Lit::Num(n) => Ok((
                vec![],
                Type::Num,
                ctx.clone(),
                Node::new(
                    Expr::Lit {
                        lit: Lit::Num(n.clone()),
                        ty: Type::Num,
                    },
                    expr.span(),
                ),
            )),
            res::Lit::Bool(b) => Ok((
                vec![],
                Type::Bool,
                ctx.clone(),
                Node::new(
                    Expr::Lit {
                        lit: Lit::Bool(b.clone()),
                        ty: Type::Bool,
                    },
                    expr.span(),
                ),
            )),
            res::Lit::String(s) => Ok((
                vec![],
                Type::String,
                ctx.clone(),
                Node::new(
                    Expr::Lit {
                        lit: Lit::String(s.clone()),
                        ty: Type::String,
                    },
                    expr.span(),
                ),
            )),
        },
        res::Expr::Ident(name) => {
            if let Some(scm) = ctx.clone().vars.get(&name) {
                let ty = instantiate(scm.clone());
                Ok((
                    vec![],
                    ty.clone(),
                    ctx.clone(),
                    Node::new(Expr::Ident { name, ty }, expr.span()),
                ))
            } else {
                Err(TypeError::from(format!(
                    "unbound variable: {:?} - \"{}\"",
                    expr,
                    src[name.span()].to_string()
                )))
            }
        }
        res::Expr::Lambda { params, body } => {
            let mut ty_binders = vec![];
            let mut tmp_ctx = ctx.clone();
            let mut new_params = vec![];
            for p in params.clone() {
                let ty_binder = Type::Var(TyVar::fresh());
                ty_binders.push(ty_binder.clone());
                let (cs, ty, ctx, pat) = infer_pattern(src, ctx, p)?;
                tmp_ctx = ctx.union(tmp_ctx);
                new_params.push(pat);
            }
            let (cs, t, c, e) = infer_expr(src, &mut tmp_ctx, body)?;
            let ty = Type::Lambda(ty_binders.clone(), Box::new(t.clone()));
            Ok((
                cs,
                ty.clone(),
                tmp_ctx.union(c),
                Node::new(
                    Expr::Lambda {
                        params: new_params,
                        body: e,
                        ty,
                    },
                    expr.span(),
                ),
            ))
        }
        res::Expr::Apply { fun, args } => {
            let (mut cs1, t1, mut ctx1, e1) = infer_expr(src, ctx, fun)?;
            let mut new_args = vec![];
            let mut ty_args = vec![];
            for a in args {
                let (cs2, t2, ctx2, e2) = infer_expr(src, &mut ctx1, a)?;
                ctx1 = ctx2.union(ctx1);
                cs1 = cs1.into_iter().chain(cs2.into_iter()).collect();
                new_args.push(e2);
                ty_args.push(t2);
            }
            let ty_ret = Type::Var(TyVar::fresh());
            cs1.push(Constraint::Eq(
                t1.clone(),
                Type::Lambda(ty_args, Box::new(ty_ret.clone())),
            ));
            // println!("t1: {:?}", t1.clone());
            // println!("e1: {:?}", e1.clone());
            Ok((
                cs1,
                ty_ret.clone(),
                ctx1,
                Node::new(
                    Expr::Apply {
                        fun: e1,
                        args: new_args,
                        ty: ty_ret,
                    },
                    expr.span(),
                ),
            ))
        }
        res::Expr::Let { pat, expr, body } => {
            let (cs1, t1, mut ctx1, e1) = infer_expr(src, ctx, expr.clone())?;
            let scheme = generalize(ctx1.clone(), t1.clone());
            let mut tmp_ctx = ctx1.clone();
            // tmp_ctx.extend(*name, scheme);
            let (cs1, t1, mut ctx1, pat) = infer_pattern(src, &mut tmp_ctx, pat)?;
            let (cs2, t2, ctx2, e2) = infer_expr(src, &mut tmp_ctx, body)?;
            ctx1 = ctx2.union(ctx1);
            let cs = cs1.into_iter().chain(cs2.into_iter()).collect::<Vec<_>>();
            Ok((
                cs,
                t2.clone(),
                ctx1,
                Node::new(
                    Expr::Let {
                        pat,
                        expr: e1,
                        body: e2,
                        ty: t2,
                    },
                    expr.span(),
                ),
            ))
        }
        res::Expr::Fn {
            name,
            params,
            expr,
            body,
        } => {
            let mut e_ctx = ctx.clone();
            let f_var = Type::Var(TyVar::fresh());
            e_ctx.extend(*name, Scheme::new(vec![], f_var.clone()));

            let mut ty_binders = vec![];
            let mut tmp_ctx = e_ctx.clone();
            let mut new_params = vec![];
            for p in params.clone() {
                let ty_binder = Type::Var(TyVar::fresh());
                ty_binders.push(ty_binder.clone());
                let (cs, ty, ctx, new_p) = infer_pattern(src, &mut tmp_ctx, p)?;
                tmp_ctx = ctx.union(tmp_ctx);
                new_params.push(new_p);
            }
            let (cs1, t1, c, expr) = infer_expr(src, &mut tmp_ctx, expr)?;
            let ty = Type::Lambda(ty_binders.clone(), Box::new(t1.clone()));
            tmp_ctx.extend(*name, Scheme::new(vec![], ty.clone()));

            let (cs2, t2, c2, e2) = infer_expr(src, &mut tmp_ctx, body)?;
            Ok((
                cs1.into_iter().chain(cs2.into_iter()).collect(),
                t2.clone(),
                tmp_ctx.union(c).union(c2),
                Node::new(
                    Expr::Fn {
                        name,
                        params: new_params,
                        expr: expr.clone(),
                        body: e2,
                        ty: t2,
                    },
                    expr.span(),
                ),
            ))
        }
        res::Expr::Match { expr, cases } => {
            let (mut cs1, t1, mut ctx1, e1) = infer_expr(src, ctx, expr.clone())?;
            let mut new_cases = vec![];
            let ty_ret = Type::Var(TyVar::fresh());
            for c in cases {
                let (csp, tp, mut ctxp, p) =
                    infer_pattern(src, &mut ctx1, c.inner().pattern.clone())?;
                let (cse, te, ctxe, e) = infer_expr(src, &mut ctxp, c.inner().expr.clone())?;
                cs1 = cs1
                    .into_iter()
                    .chain(csp.into_iter())
                    .chain(cse.into_iter())
                    .collect();
                cs1.push(Constraint::Eq(t1.clone(), tp));
                cs1.push(Constraint::Eq(te, ty_ret.clone()));
                ctx1 = ctxe.union(ctxp).union(ctx1);
                new_cases.push(Node::new(
                    MatchCase {
                        pattern: p,
                        expr: e,
                    },
                    c.span(),
                ));
            }
            Ok((
                cs1,
                ty_ret.clone(),
                ctx1,
                Node::new(
                    Expr::Match {
                        expr: e1,
                        cases: new_cases,
                        ty: ty_ret,
                    },
                    expr.span(),
                ),
            ))
        }
        res::Expr::If { cond, then, else_ } => {
            // println!("ctx: {:?}", ctx);
            let (mut cs1, t1, mut ctx1, e1) = infer_expr(src, ctx, cond)?;
            // println!("ctx1: {:?}", ctx1);
            let (cs2, t2, mut ctx2, e2) = infer_expr(src, &mut ctx1, then)?;
            // println!("ctx2: {:?}", ctx2);
            let (cs3, t3, ctx3, e3) = infer_expr(src, &mut ctx2, else_)?;
            // println!("ctx3: {:?}", ctx3);
            ctx1 = ctx3.union(ctx2).union(ctx1);
            cs1 = cs1
                .into_iter()
                .chain(cs2.into_iter())
                .chain(cs3.into_iter())
                .collect();
            cs1.push(Constraint::Eq(t1, Type::Bool));
            cs1.push(Constraint::Eq(t2.clone(), t3));
            Ok((
                cs1,
                t2.clone(),
                ctx1,
                Node::new(
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
        res::Expr::Unit => Ok((
            vec![],
            Type::Unit,
            ctx.clone(),
            Node::new(Expr::Unit, expr.span()),
        )),
    }
}

fn infer_pattern(
    src: &str,
    ctx: &mut Context,
    pattern: Node<res::Pattern>,
) -> InferResult<(Vec<Constraint>, Type, Context, Node<Pattern>)> {
    match pattern.inner().clone() {
        res::Pattern::Lit(lit) => match lit {
            res::Lit::Num(n) => Ok((
                vec![],
                Type::Num,
                ctx.clone(),
                Node::new(Pattern::Lit(Lit::Num(n.clone())), pattern.span().clone()),
            )),
            res::Lit::Bool(b) => Ok((
                vec![],
                Type::Bool,
                ctx.clone(),
                Node::new(Pattern::Lit(Lit::Bool(b.clone())), pattern.span().clone()),
            )),
            res::Lit::String(s) => Ok((
                vec![],
                Type::String,
                ctx.clone(),
                Node::new(Pattern::Lit(Lit::String(s.clone())), pattern.span().clone()),
            )),
        },
        res::Pattern::Ident(name) => {
            if let Some(scm) = ctx.clone().vars.get(&name) {
                let ty = instantiate(scm.clone());
                ctx.extend(name, Scheme::new(vec![], ty.clone()));
                Ok((
                    vec![],
                    ty.clone(),
                    ctx.clone(),
                    Node::new(Pattern::Ident(name), pattern.span().clone()),
                ))
            } else {
                Err(TypeError::from(format!(
                    "unbound variable: {:?} - \"{}\"",
                    pattern,
                    src[pattern.span()].to_string()
                )))
            }
        }
        res::Pattern::Wildcard => Ok((
            vec![],
            Type::Var(TyVar::fresh()),
            ctx.clone(),
            Node::new(Pattern::Wildcard, pattern.span().clone()),
        )),
        res::Pattern::Unit => Ok((
            vec![],
            Type::Unit,
            ctx.clone(),
            Node::new(Pattern::Unit, pattern.span().clone()),
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

fn apply_subst_item(subst: Substitution, item: Node<Item>) -> Node<Item> {
    Node::new(
        match item.inner().clone() {
            Item::Expr(expr) => Item::Expr(
                apply_subst_expr(subst, Node::new(expr, item.span()))
                    .inner()
                    .clone(),
            ),
            Item::Def { pat, expr, ty } => Item::Def {
                pat,
                expr: apply_subst_expr(subst.clone(), expr),
                ty: apply_subst(subst, ty),
            },
            Item::Fn {
                name,
                params,
                body,
                ty,
            } => Item::Fn {
                name,
                params,
                body: apply_subst_expr(subst.clone(), body),
                ty: apply_subst(subst, ty),
            },
        },
        item.span(),
    )
}

fn apply_subst_expr(subst: Substitution, expr: Node<Expr>) -> Node<Expr> {
    Node::new(
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
                pat,
                expr,
                body,
                ty,
            } => Expr::Let {
                pat,
                expr: apply_subst_expr(subst.clone(), expr),
                body: apply_subst_expr(subst.clone(), body),
                ty: apply_subst(subst, ty),
            },
            Expr::Fn {
                name,
                params,
                expr,
                body,
                ty,
            } => Expr::Fn {
                name,
                params,
                expr: apply_subst_expr(subst.clone(), expr),
                body: apply_subst_expr(subst.clone(), body),
                ty: apply_subst(subst, ty),
            },
            Expr::Match { expr, cases, ty } => Expr::Match {
                expr: apply_subst_expr(subst.clone(), expr),
                cases: cases
                    .into_iter()
                    .map(|case| {
                        Node::new(
                            MatchCase {
                                pattern: case.inner().pattern.clone(),
                                expr: apply_subst_expr(subst.clone(), case.inner().clone().expr),
                            },
                            case.span(),
                        )
                    })
                    .collect(),
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
            Expr::Unit => Expr::Unit,
        },
        expr.span(),
    )
}

pub fn type_inference<'src>(
    src: &'src str,
    ctx: &mut Context,
    root: Node<res::Root>,
) -> InferResult<(Node<Root>, Context)> {
    let mut s = Substitution::new();
    let mut items = vec![];
    let mut ctx_ret = Context::new();
    // println!("top ctx: {:?}", ctx);
    // if let Some(scm) = ctx.clone().vars.get(&UniqueId::from(19)) {
    //     let ty = instantiate(scm.clone());
    //     println!("ty: {:?}", ty);
    //     ctx_ret.extend(UniqueId::from(0), Scheme::new(vec![], ty));
    // } else {
    //     panic!("unbound variable: {:?}", UniqueId::from(19));
    // }
    for item in root.inner().items.clone() {
        let (cs, ctx, i) = infer_item(src, ctx, item)?;
        // types.push(apply_subst(subst, ty).lower(&mut HashMap::new()));
        for c in cs {
            match c {
                Constraint::Eq(t1, t2) => {
                    s = unify(apply_subst(s.clone(), t1), apply_subst(s.clone(), t2))?.compose(s);
                }
            }
        }
        ctx_ret = ctx_ret.union(ctx);
        items.push(i);
    }
    // println!("s: {:?}", s);
    // println!("ctx_ret: {:#?}", ctx_ret);
    // println!("ctx_ret subs: {:#?}", ctx_ret.apply_subst(s.clone()));
    Ok((
        Node::new(
            apply_subst_root(s.clone(), Root { items }),
            root.span().clone(),
        ),
        // Node::new(Root { items }, root.span().clone()),
        ctx_ret.apply_subst(s),
    ))
}

// mod tests {
//     use super::res::resolve;
//     use crate::{analysis::res, syntax::parse::parse, util::node::Node};

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
//     //         items: vec![Node::new(res::Item::Def {
//     //             name: Node::new(0.into(), 0..0),
//     //             expr: Node::new(
//     //                 res::Expr::Apply {
//     //                     fun: Box::new(Node::new(res::Expr::Ident(0.into()), 0..0)),
//     //                     args: vec![Node::new(res::Expr::Ident(0.into()), 0..0)],
//     //                 },
//     //                 0..0,
//     //             ),
//     //         })],
//     //     };
//     // }
// }
