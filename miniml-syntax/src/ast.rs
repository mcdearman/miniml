use crate::lex::TokenKind;
use itertools::join;
use miniml_util::{intern::InternedString, span::Spanned};
use num_complex::Complex64;
use num_rational::Rational64;
use std::fmt::{Debug, Display};

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    pub decls: Vec<Spanned<Decl>>,
}

impl Display for Root {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            join(self.clone().decls.into_iter().map(|d| d.value), "\n")
        )
    }
}

// impl Debug for Root {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         write!(f, "{:?}", join(self.decls.clone().into_iter(), "\n"))
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

impl Display for Decl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Decl::Const { name, expr } => write!(f, "const {} = {}", name.value, expr.value),
            Decl::Let { name, expr } => write!(f, "let {} = {}", name.value, expr.value),
            Decl::Fn { name, params, body } => {
                write!(
                    f,
                    "fn {} {} = {}",
                    name.value,
                    join(params.into_iter().map(|p| p.value), ", "),
                    body.value
                )
            }
        }
    }
}

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

impl Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        // debug prints but repeat indent
        // match self.clone() {
        //     Expr::Ident(i) => write!(f, "{}Indent({:?})", " ".repeat(indent), i),
        //     Expr::Lit(l) => write!(f, "{:?}", l),
        //     Expr::Prefix { op, expr } => {
        //         write!(
        //             f,
        //             "{}Prefix{:?}{:?}",
        //             " ".repeat(indent),
        //             op.value,
        //             expr.value
        //         )?;
        //         expr.value.fmt(f, indent + 2)?;
        //         write!(f, "{}", " ".repeat(indent))
        //     }
        //     Expr::Infix { op, lhs, rhs } => {
        //         write!(f, "Expr\n{:?} {:?} {:?}", lhs.value, op.value, rhs.value)
        //     }
        //     Expr::Unit => write!(f, "{}Unit", " ".repeat(indent)),
        //     _ => todo!(),
        // }
        todo!()
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Expr::Ident(i) => write!(f, "{}", i),
            Expr::Lit(l) => write!(f, "{}", l),
            Expr::Prefix { op, expr } => write!(f, "{}{}", op.value, expr.value),
            Expr::Infix { op, lhs, rhs } => write!(f, "{} {} {}", lhs.value, op.value, rhs.value),
            Expr::Let { name, expr, body } => {
                write!(f, "let {} = {} in {}", name.value, expr.value, body.value)
            }
            Expr::Apply { fun, args } => {
                write!(f, "({}", fun.value)?;
                for arg in args {
                    write!(f, " {}", arg.value)?;
                }
                write!(f, ")")
            }
            Expr::If { cond, then, else_ } => {
                write!(
                    f,
                    "if {} then {} else {}",
                    cond.value, then.value, else_.value
                )
            }
            Expr::Lambda { params, body } => write!(
                f,
                "\\{} => {}",
                join(params.into_iter().map(|p| p.value), " "),
                body.value
            ),
            Expr::Unit => write!(f, "()"),
            Expr::Error => write!(f, "error"),
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
    fn from(kind: TokenKind) -> Self {
        match kind {
            TokenKind::Minus => PrefixOp::Neg,
            TokenKind::Not => PrefixOp::Not,
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
    Stmt,
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
            InfixOp::Stmt => write!(f, ";"),
        }
    }
}

impl From<TokenKind> for InfixOp {
    fn from(kind: TokenKind) -> Self {
        match kind {
            TokenKind::Plus => InfixOp::Add,
            TokenKind::Minus => InfixOp::Sub,
            TokenKind::Star => InfixOp::Mul,
            TokenKind::Slash => InfixOp::Div,
            TokenKind::Percent => InfixOp::Rem,
            TokenKind::Caret => InfixOp::Pow,
            TokenKind::Eq => InfixOp::Eq,
            TokenKind::Neq => InfixOp::Neq,
            TokenKind::Lt => InfixOp::Lt,
            TokenKind::Gt => InfixOp::Gt,
            TokenKind::Leq => InfixOp::Leq,
            TokenKind::Geq => InfixOp::Geq,
            TokenKind::And => InfixOp::And,
            TokenKind::Or => InfixOp::Or,
            TokenKind::Pipe => InfixOp::Pipe,
            _ => panic!("Not an infix operator: {:?}", kind),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
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
