use super::{token::TokenKind, ParserError};
use crate::{intern::InternedString, list::List, T};
use num_bigint::BigInt;
use num_complex::Complex64;
use num_rational::Rational64;
use std::{collections::HashMap, fmt::Display};

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
pub struct Decl {
    pub pattern: Pattern,
    pub value: Box<Expr>,
}

impl Display for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {}", self.pattern, self.value)
    }
}

/// A dummy type to store a let-binding that may be either a declaration or an expression.
#[derive(Debug, Clone, PartialEq)]
pub enum LetKind {
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
    Unit,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Ident(i) => write!(f, "{}", i),
            Expr::Lit(l) => write!(f, "{}", l),
            Expr::Prefix { op, expr } => write!(f, "({} {})", op, expr),
            Expr::Infix { op, lhs, rhs } => write!(f, "({} {} {})", lhs, op, rhs),
            Expr::Let(l) => write!(f, "{}", l),
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
            Expr::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Int(Int),
    Real(Real),
    Complex(Complex64),
    String(InternedString),
    Char(char),
    Bool(bool),
    List(List<Expr>),
    Tuple(Tuple),
    Map(Map),
    Record(Record),
    Lambda(Lambda),
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Real(r) => write!(f, "{}", r),
            Lit::Complex(c) => write!(f, "{}", c),
            Lit::String(s) => write!(f, "{}", s),
            Lit::Char(i) => write!(f, "{}", i),
            Lit::Bool(i) => write!(f, "{}", i),
            Lit::List(l) => write!(f, "{}", l),
            Lit::Tuple(t) => write!(f, "{:?}", t),
            Lit::Map(m) => write!(f, "{}", m),
            Lit::Record(r) => write!(f, "{}", r),
            Lit::Lambda(l) => write!(f, "{}", l),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Int {
    pub value: BigInt,
    pub radix: Radix,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Radix {
    Bin,
    Oct,
    Dec,
    Hex,
}

impl Display for Int {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.radix {
            Radix::Bin => write!(f, "0b{:b}", self.value),
            Radix::Oct => write!(f, "0o{:o}", self.value),
            Radix::Dec => write!(f, "{}", self.value),
            Radix::Hex => write!(f, "0x{:x}", self.value),
        }
    }
}

impl TryFrom<String> for Int {
    type Error = ParserError;

    fn try_from(s: String) -> std::result::Result<Self, Self::Error> {
        if s.starts_with("0b") {
            Ok(Self {
                value: BigInt::parse_bytes(
                    s.strip_prefix("0b")
                        .expect("expected integer prefix")
                        .as_bytes(),
                    2,
                )
                .ok_or(ParserError::new("Invalid binary integer literal"))?,
                radix: Radix::Bin,
            })
        } else if s.starts_with("0o") {
            Ok(Self {
                value: BigInt::parse_bytes(
                    s.strip_prefix("0o")
                        .expect("expected integer prefix")
                        .as_bytes(),
                    8,
                )
                .ok_or(ParserError::new("Invalid octal integer literal"))?,
                radix: Radix::Oct,
            })
        } else if s.starts_with("0x") {
            Ok(Self {
                value: BigInt::parse_bytes(
                    s.strip_prefix("0x")
                        .expect("expected integer prefix")
                        .as_bytes(),
                    16,
                )
                .ok_or(ParserError::new("Invalid hexadecimal integer literal"))?,
                radix: Radix::Hex,
            })
        } else {
            Ok(Self {
                value: BigInt::parse_bytes(s.as_bytes(), 10)
                    .ok_or(ParserError::new("Invalid decimal integer literal"))?,
                radix: Radix::Dec,
            })
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Real(pub f64);

impl Display for Real {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<String> for Real {
    fn from(s: String) -> Self {
        Self(s.parse().expect("Invalid float"))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    pub items: Vec<Expr>,
}

impl Display for Tuple {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "(")?;
        while let Some(item) = self.items.clone().into_iter().next() {
            write!(f, "{}", item)?;
        }
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
    fn from(token: TokenKind) -> Self {
        match token {
            T![-] => PrefixOp::Neg,
            T![!] => PrefixOp::Not,
            _ => panic!("Not a prefix operator: {:?}", token),
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
        }
    }
}

impl From<TokenKind> for InfixOp {
    fn from(token: TokenKind) -> Self {
        match token {
            T![+] => InfixOp::Add,
            T![-] => InfixOp::Sub,
            T![*] => InfixOp::Mul,
            T![/] => InfixOp::Div,
            T![%] => InfixOp::Mod,
            T![^] => InfixOp::Pow,
            T![=] => InfixOp::Eq,
            T![!=] => InfixOp::Neq,
            T![<] => InfixOp::Lss,
            T![>] => InfixOp::Gtr,
            T![<=] => InfixOp::Leq,
            T![>=] => InfixOp::Geq,
            T![&&] => InfixOp::And,
            T![||] => InfixOp::Or,
            _ => panic!("Not an infix operator: {:?}", token),
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
pub enum Pattern {
    Ident(InternedString),
    Int(Int),
    BigInt(BigInt),
    Rational(Rational64),
    Bool(bool),
    String(InternedString),
    Char(char),
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
            Pattern::BigInt(i) => write!(f, "{}", i),
            Pattern::Rational(r) => write!(f, "{}", r),
            Pattern::Bool(b) => write!(f, "{}", b),
            Pattern::String(s) => write!(f, "{}", s),
            Pattern::Char(c) => write!(f, "{}", c),
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
