use crate::lex::Token;
use chumsky::span::SimpleSpan;
use itertools::join;
use miniml_util::intern::InternedString;
use num_complex::Complex64;
use num_rational::Rational64;
use std::fmt::{Debug, Display};

pub type Span = SimpleSpan;
pub type Spanned<T> = (T, Span);

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    pub decls: Vec<Spanned<Decl>>,
}

// impl Display for Root {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "{}",
//             join(self.clone().decls.into_iter().map(|d| d.0), "\n")
//         )
//     }
// }

// impl Debug for Root {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(
//             f,
//             "{:?}",
//             join(self.decls.clone().into_iter().map(|(d, s)| d), "\n")
//         )
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Const {
        name: Spanned<InternedString>,
        expr: Box<Spanned<Expr>>,
    },
    Let {
        name: Spanned<InternedString>,
        expr: Box<Spanned<Expr>>,
    },
    Fn {
        name: Spanned<InternedString>,
        params: Vec<Spanned<InternedString>>,
        body: Box<Spanned<Expr>>,
    },
}

// impl Display for Decl {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Decl::Const { name, expr } => write!(f, "const {} = {}", name.0, expr.0),
//             Decl::Let { name, expr } => write!(f, "let {} = {}", name.0, expr.0),
//             Decl::Fn { name, params, body } => {
//                 write!(
//                     f,
//                     "fn {} {} = {}",
//                     name.0,
//                     join(params.into_iter().map(|p| p.0), ", "),
//                     body.0
//                 )
//             }
//         }
//     }
// }

// impl Debug for Decl {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Decl::Const { name, expr } => write!(f, "Const({:?} = {:?})", name, expr),
//             Decl::Let { name, expr } => write!(f, "Let({:?} = {:?})", name, expr),
//             Decl::Fn { name, params, body } => {
//                 write!(f, "Fn({:?} {:?} = {:?})", name, join(params, " "), body)
//             }
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(InternedString),
    Lit(Lit),
    Prefix {
        op: Spanned<PrefixOp>,
        expr: Box<Spanned<Self>>,
    },
    Infix {
        op: Spanned<InfixOp>,
        lhs: Box<Spanned<Self>>,
        rhs: Box<Spanned<Self>>,
    },
    Let {
        name: Spanned<InternedString>,
        expr: Box<Spanned<Self>>,
        body: Box<Spanned<Self>>,
    },
    Apply {
        fun: Box<Spanned<Self>>,
        args: Vec<Spanned<Self>>,
    },
    If {
        cond: Box<Spanned<Self>>,
        then: Box<Spanned<Self>>,
        else_: Box<Spanned<Self>>,
    },
    Lambda {
        params: Vec<Spanned<InternedString>>,
        body: Box<Spanned<Self>>,
    },
    Unit,
    Error,
}

// impl Expr {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
//         // debug prints but repeat indent
//         match self.clone() {
//             Expr::Ident(i) => write!(f, "{}Indent({:?})", " ".repeat(indent), i),
//             Expr::Lit(l) => write!(f, "{:?}", l),
//             Expr::Prefix { op, expr } => {
//                 write!(f, "{}Prefix{:?}{:?}", " ".repeat(indent), op.0, expr.0)
//             }
//             Expr::Infix { op, lhs, rhs } => {
//                 write!(f, "{:?} {:?} {:?}", lhs.0, op.0, rhs.0)
//             }
//             _ => todo!(),
//         }
//     }
// }

// impl Display for Expr {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self.clone() {
//             Expr::Ident(i) => write!(f, "{}", i),
//             Expr::Lit(l) => write!(f, "{}", l),
//             Expr::Prefix { op, expr } => write!(f, "{}{}", op.0, expr.0),
//             Expr::Infix { op, lhs, rhs } => write!(f, "{} {} {}", lhs.0, op.0, rhs.0),
//             Expr::Let { name, expr, body } => {
//                 write!(f, "let {} = {} in {}", name.0, expr.0, body.0)
//             }
//             Expr::Apply { fun, args } => {
//                 write!(f, "({}", fun.0)?;
//                 for arg in args {
//                     write!(f, " {}", arg.0)?;
//                 }
//                 write!(f, ")")
//             }
//             Expr::If { cond, then, else_ } => {
//                 write!(f, "if {} then {} else {}", cond.0, then.0, else_.0)
//             }
//             Expr::Lambda { params, body } => write!(
//                 f,
//                 "\\{} => {}",
//                 join(params.into_iter().map(|p| p.0), " "),
//                 body.0
//             ),
//             Expr::Unit => write!(f, "()"),
//             Expr::Error => write!(f, "error"),
//         }
//     }
// }

// impl Debug for Expr {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self.clone() {
//             Self::Ident(name) => write!(f, "Ident({:?})", name),
//             Self::Lit(l) => write!(f, "Lit({:?})", l),
//             Self::Prefix { op, expr } => write!(f, "Prefix({:?}{:?})", op, expr),
//             Self::Infix { op, lhs, rhs } => write!(f, "Infix({:?} {:?} {:?})", lhs, op, rhs),
//             Self::Let { name, expr, body } => {
//                 write!(f, "Let({:?} = {:?} in {:?})", name, expr, body)
//             }
//             Self::Apply { fun, args } => write!(f, "Apply({:?} {:?})", fun, join(args, " ")),
//             Self::If { cond, then, else_ } => write!(f, "If({:?} {:?} {:?})", cond, then, else_),
//             Self::Lambda { params, body } => {
//                 write!(f, "Lambda({:?} -> {:?})", join(params, " "), body)
//             }
//             Self::Unit => write!(f, "Unit"),
//             Self::Error => write!(f, "Error"),
//         }
//     }
// }

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

impl From<Token> for PrefixOp {
    fn from(kind: Token) -> Self {
        match kind {
            Token::Minus => PrefixOp::Neg,
            Token::Not => PrefixOp::Not,
            _ => panic!("Not a prefix operator: {:?}", kind),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InfixOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
    Eq,
    Neq,
    Lt,
    Gt,
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
            InfixOp::Rem => write!(f, "%"),
            InfixOp::Pow => write!(f, "^"),
            InfixOp::Eq => write!(f, "="),
            InfixOp::Neq => write!(f, "!="),
            InfixOp::Lt => write!(f, "<"),
            InfixOp::Gt => write!(f, ">"),
            InfixOp::Leq => write!(f, "<="),
            InfixOp::Geq => write!(f, ">="),
            InfixOp::And => write!(f, "&&"),
            InfixOp::Or => write!(f, "||"),
            InfixOp::Pipe => write!(f, "|>"),
        }
    }
}

impl From<Token> for InfixOp {
    fn from(kind: Token) -> Self {
        match kind {
            Token::Plus => InfixOp::Add,
            Token::Minus => InfixOp::Sub,
            Token::Star => InfixOp::Mul,
            Token::Slash => InfixOp::Div,
            Token::Percent => InfixOp::Rem,
            Token::Caret => InfixOp::Pow,
            Token::Eq => InfixOp::Eq,
            Token::Neq => InfixOp::Neq,
            Token::Lt => InfixOp::Lt,
            Token::Gt => InfixOp::Gt,
            Token::Leq => InfixOp::Leq,
            Token::Geq => InfixOp::Geq,
            Token::And => InfixOp::And,
            Token::Or => InfixOp::Or,
            Token::Pipe => InfixOp::Pipe,
            _ => panic!("Not an infix operator: {:?}", kind),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Lit {
    Int(i64),
    Rational(Rational64),
    Real(f64),
    Complex(Complex64),
    Char(char),
    String(InternedString),
}

impl Display for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "{}", i),
            Lit::Rational(r) => write!(f, "{}", r),
            Lit::Real(r) => write!(f, "{}", r),
            Lit::Complex(c) => write!(f, "{}", c),
            Lit::Char(c) => write!(f, "'{}'", c),
            Lit::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

impl Debug for Lit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lit::Int(i) => write!(f, "Int({})", i),
            Lit::Rational(r) => write!(f, "Rational({})", r),
            Lit::Real(r) => write!(f, "Real({})", r),
            Lit::Complex(c) => write!(f, "{}", c),
            Lit::Char(c) => write!(f, "'{}'", c),
            Lit::String(s) => write!(f, "String({})", s),
        }
    }
}
