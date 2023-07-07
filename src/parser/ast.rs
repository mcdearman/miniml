use crate::intern::InternedString;
use itertools::join;
use num_bigint::BigInt;
use num_complex::Complex64;
use num_rational::{BigRational, Rational32, Rational64};
use std::{collections::HashMap, fmt::Display, str::FromStr};

use super::{error::Error, token::TokenKind};

#[derive(Debug, Clone, PartialEq)]
pub struct File {
    pub items: Vec<Item>,
}

impl Display for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", join(self.clone().items, "\n"))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Data(Data),
    Decl(Decl),
    Expr(Expr),
}

impl Display for Item {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Item::Data(data) => write!(f, "{}", data),
            Item::Decl(decl) => write!(f, "{}", decl),
            Item::Expr(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Data {
    pub name: InternedString,
    pub fields: Vec<InternedString>,
}

impl Display for Data {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "data {} {}", self.name, self.fields.join(" "))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Let(LetDecl),
    Fn(FnDecl),
    Error,
}

impl Display for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Decl::Let(l) => write!(f, "{}", l),
            Decl::Fn(fun) => write!(f, "{}", fun),
            Decl::Error => write!(f, "error"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetDecl {
    pub pattern: Pattern,
    pub value: Box<Expr>,
}

impl Display for LetDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {}", self.pattern, self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl {
    pub name: InternedString,
    pub value: Box<Expr>,
}

impl Display for FnDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {} = {}", self.name, self.value)
    }
}

/// A dummy type to store a let-binding that may be either a declaration or an expression.
#[derive(Debug, Clone, PartialEq)]
pub enum LetKind {
    Decl(Decl),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum FnKind {
    Decl(Decl),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(InternedString),
    Lit(Lit),
    Prefix {
        op: PrefixOp,
        expr: Box<Self>,
    },
    Infix {
        op: InfixOp,
        lhs: Box<Self>,
        rhs: Box<Self>,
    },
    Let(LetExpr),
    Fn(FnExpr),
    Apply(Box<Self>, Box<Self>),
    If {
        cond: Box<Self>,
        then: Box<Self>,
        else_: Box<Self>,
    },
    Match {
        expr: Box<Self>,
        arms: Vec<MatchArm>,
    },
    List(Vec<Expr>),
    Tuple(Tuple),
    Map(Map),
    Record(Record),
    Lambda(Lambda),
    Unit,
    Error,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Ident(i) => write!(f, "{}", i),
            Expr::Lit(l) => write!(f, "{}", l),
            Expr::Prefix { op, expr } => write!(f, "({} {})", op, expr),
            Expr::Infix { op, lhs, rhs } => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::Let(l) => write!(f, "{}", l),
            Expr::Fn(fun) => write!(f, "{}", fun),
            Expr::Apply(lhs, rhs) => write!(f, "({} {})", lhs, rhs),
            Expr::If { cond, then, else_ } => {
                write!(f, "(if {} then {} else {})", cond, then, else_)
            }
            Expr::Match { expr, arms } => {
                write!(f, "(match {} with ", expr)?;
                for arm in arms {
                    write!(f, "{}", arm)?;
                }
                write!(f, ")")
            }
            Expr::List(l) => write!(f, "{:?}", l.clone()),
            Expr::Tuple(t) => write!(f, "{}", t),
            Expr::Map(m) => write!(f, "{}", m),
            Expr::Record(r) => write!(f, "{}", r),
            Expr::Lambda(l) => write!(f, "{}", l),
            Expr::Unit => write!(f, "()"),
            Expr::Error => write!(f, "error"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(BigInt),
    Rational(BigRational),
    Real(f64),
    Complex(Complex64),
    String(InternedString),
    Char(char),
    Bool(bool),
    Error,
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Rational(r) => write!(f, "{}", r),
            Lit::Real(r) => write!(f, "{}", r),
            Lit::Complex(c) => write!(f, "{}", c),
            Lit::String(s) => write!(f, "{}", s),
            Lit::Char(i) => write!(f, "{}", i),
            Lit::Bool(i) => write!(f, "{}", i),
            Lit::Error => write!(f, "error"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    pub items: Vec<Expr>,
}

impl Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        write!(f, "{}", join(self.clone().items, ", "))?;
        write!(f, ")")
    }
}

impl IntoIterator for Tuple {
    type Item = Expr;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Map {
    pub items: HashMap<InternedString, Expr>,
}

impl Display for Map {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let map = self.clone();
        write!(f, "{{")?;
        while let Some((key, value)) = map.items.clone().into_iter().next() {
            write!(f, "{}: {}", key, value)?;
        }
        write!(f, "}}")
    }
}

impl IntoIterator for Map {
    type Item = (InternedString, Expr);
    type IntoIter = std::collections::hash_map::IntoIter<InternedString, Expr>;

    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Record {
    name: InternedString,
    fields: HashMap<InternedString, Expr>,
}

impl Display for Record {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let struct_ = self.clone();
        write!(f, "{} {{", struct_.name)?;
        for (key, value) in struct_.fields.clone() {
            write!(f, "{}: {}", key, value)?;
            if struct_.fields.keys().last().unwrap() != &key {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}

impl IntoIterator for Record {
    type Item = (InternedString, Expr);
    type IntoIter = std::collections::hash_map::IntoIter<InternedString, Expr>;

    fn into_iter(self) -> Self::IntoIter {
        self.fields.into_iter()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lambda {
    pub param: Pattern,
    pub body: Box<Expr>,
}

impl Display for Lambda {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\\{} -> {}", self.param, self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub expr: Expr,
}

impl Display for MatchArm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} => {}", self.pattern, self.expr)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfBuilder {
    pub cond: Expr,
    pub then: Expr,
    pub elifs: Vec<(Expr, Expr)>,
}

impl IfBuilder {
    pub fn new(cond: Expr, then: Expr) -> Self {
        Self {
            cond,
            then,
            elifs: vec![],
        }
    }

    pub fn elif(mut self, cond: Expr, then: Expr) -> Self {
        self.elifs.push((cond, then));
        self
    }

    pub fn else_(self, else_: Expr) -> Expr {
        Expr::If {
            cond: Box::new(self.cond),
            then: Box::new(self.then),
            else_: Box::new(else_),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrefixOp {
    Neg,
    Not,
}

impl Display for PrefixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PrefixOp::Neg => write!(f, "-"),
            PrefixOp::Not => write!(f, "!"),
        }
    }
}

impl From<TokenKind> for PrefixOp {
    fn from(TokenKind: TokenKind) -> Self {
        match TokenKind {
            TokenKind::Sub => PrefixOp::Neg,
            TokenKind::Not => PrefixOp::Not,
            _ => panic!("Not a prefix operator: {:?}", TokenKind),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
    Eq,
    Neq,
    Lss,
    Gtr,
    Leq,
    Geq,
    And,
    Or,
    Pipe,
}

impl Display for InfixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixOp::Add => write!(f, "+"),
            InfixOp::Sub => write!(f, "-"),
            InfixOp::Mul => write!(f, "*"),
            InfixOp::Div => write!(f, "/"),
            InfixOp::Mod => write!(f, "%"),
            InfixOp::Pow => write!(f, "^"),
            InfixOp::Eq => write!(f, "="),
            InfixOp::Neq => write!(f, "!="),
            InfixOp::Lss => write!(f, "<"),
            InfixOp::Gtr => write!(f, ">"),
            InfixOp::Leq => write!(f, "<="),
            InfixOp::Geq => write!(f, ">="),
            InfixOp::And => write!(f, "&&"),
            InfixOp::Or => write!(f, "||"),
            InfixOp::Pipe => write!(f, "|>"),
        }
    }
}

impl From<TokenKind> for InfixOp {
    fn from(TokenKind: TokenKind) -> Self {
        match TokenKind {
            TokenKind::Add => InfixOp::Add,
            TokenKind::Sub => InfixOp::Sub,
            TokenKind::Mul => InfixOp::Mul,
            TokenKind::Div => InfixOp::Div,
            TokenKind::Rem => InfixOp::Mod,
            TokenKind::Pow => InfixOp::Pow,
            TokenKind::Eql => InfixOp::Eq,
            TokenKind::Neq => InfixOp::Neq,
            TokenKind::Lss => InfixOp::Lss,
            TokenKind::Gtr => InfixOp::Gtr,
            TokenKind::Leq => InfixOp::Leq,
            TokenKind::Geq => InfixOp::Geq,
            TokenKind::And => InfixOp::And,
            TokenKind::Or => InfixOp::Or,
            TokenKind::Pipe => InfixOp::Pipe,
            _ => panic!("Not an infix operator: {:?}", TokenKind),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetExpr {
    pub pattern: Pattern,
    pub value: Box<Expr>,
    pub body: Box<Expr>,
}

impl Display for LetExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "(let {} = {} in {})",
            self.pattern, self.value, self.body
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnExpr {
    pub name: InternedString,
    pub value: Box<Expr>,
    pub body: Box<Expr>,
}

impl Display for FnExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(fn {} = {} in {})", self.name, self.value, self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Ident(InternedString),
    Int(BigInt),
    Rational(BigRational),
    String(InternedString),
    Char(char),
    Bool(bool),
    List(ListPattern),
    Tuple(TuplePattern),
    Map(MapPattern),
    Record(RecordPattern),
    Wildcard,
    Unit,
}

impl Display for Pattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pattern::Ident(i) => write!(f, "{}", i),
            Pattern::Int(i) => write!(f, "{}", i),
            Pattern::Rational(r) => write!(f, "{}", r),
            Pattern::String(s) => write!(f, "{}", s),
            Pattern::Char(c) => write!(f, "{}", c),
            Pattern::Bool(b) => write!(f, "{}", b),
            Pattern::List(l) => write!(f, "{}", l),
            Pattern::Tuple(t) => write!(f, "{}", t),
            Pattern::Map(m) => write!(f, "{}", m),
            Pattern::Record(r) => write!(f, "{}", r),
            Pattern::Wildcard => write!(f, "_"),
            Pattern::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ListPattern {
    pub items: Vec<Pattern>,
}

impl Display for ListPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for (i, item) in self.items.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", item)?;
        }
        write!(f, "]")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TuplePattern {
    pub items: Vec<Pattern>,
}

impl Display for TuplePattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        for (i, item) in self.items.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", item)?;
        }
        write!(f, ")")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MapPattern {
    pub items: Vec<(Pattern, Pattern)>,
}

impl Display for MapPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, (key, value)) in self.items.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", key, value)?;
        }
        write!(f, "}}")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct RecordPattern {
    pub items: Vec<(InternedString, Pattern)>,
}

impl Display for RecordPattern {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        for (i, (key, value)) in self.items.iter().enumerate() {
            if i != 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", key, value)?;
        }
        write!(f, "}}")
    }
}
