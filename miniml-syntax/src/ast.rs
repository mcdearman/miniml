use crate::{error::SyntaxError, lex::TokenKind};
use cstree::Syntax;
use itertools::join;
use miniml_util::{
    intern::InternedString,
    span::{Span, Spannable, Spanned},
};
use num_complex::Complex64;
use num_rational::Rational64;
use std::{
    fmt::{Debug, Display},
    num::ParseIntError,
    str::FromStr,
};

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
    Ident(Spanned<InternedString>),
    Lit(Spanned<Lit>),
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
        elifs: Vec<(Box<Spanned<Self>>, Box<Spanned<Self>>)>,
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
//         match self.clone() {
//             Expr::Ident(i) => write!(
//                 f,
//                 "{}Expr\n{}Indent({})",
//                 " ".repeat(indent),
//                 " ".repeat(indent + 2),
//                 i
//             ),
//             Expr::Lit(l) => write!(f, "{}", l.fmt(indent)),
//             Expr::Prefix { op, expr } => {
//                 write!(
//                     f,
//                     "{}Prefix{:?}{:?}",
//                     " ".repeat(indent),
//                     op.value,
//                     expr.value
//                 )?;
//                 expr.value.fmt(f, indent + 2)?;
//                 write!(f, "{}", " ".repeat(indent))
//             }
//             Expr::Infix { op, lhs, rhs } => {
//                 write!(f, "Expr\n{:?} {:?} {:?}", lhs.value, op.value, rhs.value)
//             }
//             Expr::Unit => write!(f, "{}Unit", " ".repeat(indent)),
//             _ => todo!(),
//         }
//     }
// }

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            Expr::Ident(i) => write!(f, "{}", i),
            Expr::Lit(l) => write!(f, "{}", l.value),
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
            Expr::If {
                cond,
                then,
                elifs,
                else_,
            } => {
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
    Int(Int),
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
            Lit::String(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Int(pub i64);

impl FromStr for Int {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.starts_with("0x") {
            i64::from_str_radix(&s[2..], 16).map(Int)
        } else if s.starts_with("0o") {
            i64::from_str_radix(&s[2..], 8).map(Int)
        } else if s.starts_with("0b") {
            i64::from_str_radix(&s[2..], 2).map(Int)
        } else {
            i64::from_str(s).map(Int)
        }
    }
}

impl Display for Int {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, PartialEq)]
pub struct Format<T> {
    pub indent: usize,
    pub value: T,
}

impl Debug for Format<Spanned<Root>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Root @ {}", self.value.span)?;
        for decl in &self.value.value.decls {
            let decl = Format {
                indent: self.indent + 2,
                value: decl.clone(),
            };
            write!(f, "\n{:?}", decl)?;
        }
        Ok(())
    }
}

impl Debug for Format<Spanned<Decl>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value.clone().value {
            Decl::Const { name, expr } => {
                let expr = Format {
                    indent: self.indent + 4,
                    value: *expr,
                };
                write!(
                    f,
                    "{}Decl @ {}\n{}Const @ {}\n{}{:?}\n{:?}",
                    " ".repeat(self.indent),
                    self.value.span,
                    " ".repeat(self.indent + 2),
                    name.span,
                    " ".repeat(self.indent + 4),
                    name.value,
                    expr
                )
            }
            Decl::Let { name, expr } => {
                let expr = Format {
                    indent: self.indent + 2,
                    value: *expr,
                };
                write!(
                    f,
                    "{}Decl @ {}\n{}{} = {:?}",
                    " ".repeat(self.indent),
                    self.value.span,
                    " ".repeat(self.indent + 2),
                    name.value,
                    expr
                )
            }
            Decl::Fn { name, params, body } => {
                let params = Format {
                    indent: self.indent + 4,
                    value: params.clone(),
                };
                let body = Format {
                    indent: self.indent + 4,
                    value: *body,
                };
                write!(
                    f,
                    "{}Decl @ {}\n{}Fn @ {}\n{}Ident @ {}\n{}{}{:?}\n{:?}",
                    " ".repeat(self.indent),
                    self.value.span,
                    " ".repeat(self.indent + 2),
                    self.value.span,
                    " ".repeat(self.indent + 4),
                    name.span,
                    " ".repeat(self.indent + 6),
                    name.value,
                    params,
                    body
                )
            }
        }
    }
}

impl Debug for Format<Vec<Spanned<InternedString>>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.value.is_empty() {
            write!(f, "\n")?;
        }
        for (i, param) in self.value.iter().enumerate() {
            write!(
                f,
                "{}Ident @ {}\n{}{}",
                " ".repeat(self.indent),
                param.span,
                " ".repeat(self.indent + 2),
                param.value
            )?;
            if i != self.value.len() - 1 {
                write!(f, "\n")?;
            }
        }
        Ok(())
    }
}

impl Debug for Format<Spanned<Expr>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value.clone().value {
            Expr::Ident(name) => write!(
                f,
                "{}Expr @ {}\n{}Ident @ {}\n{}{}",
                " ".repeat(self.indent),
                self.value.span,
                " ".repeat(self.indent + 2),
                self.value.span,
                " ".repeat(self.indent + 4),
                name.value
            ),
            Expr::Lit(l) => {
                let lit = Format {
                    indent: self.indent + 2,
                    value: l,
                };
                write!(
                    f,
                    "{}Expr @ {}\n{:?}",
                    " ".repeat(self.indent),
                    self.value.span,
                    // " ".repeat(self.indent),
                    lit
                )
            }
            Expr::Prefix { op, expr } => {
                let expr = Format {
                    indent: self.indent + 4,
                    value: *expr,
                };
                write!(
                    f,
                    "{}Expr @ {}\n{}Prefix @ {}\n{}{:?}\n{:?}",
                    " ".repeat(self.indent),
                    self.value.span,
                    " ".repeat(self.indent + 2),
                    self.value.span,
                    " ".repeat(self.indent + 4),
                    op,
                    expr
                )
            }
            Expr::Infix { op, lhs, rhs } => {
                let lhs = Format {
                    indent: self.indent + 4,
                    value: *lhs,
                };
                let rhs = Format {
                    indent: self.indent + 4,
                    value: *rhs,
                };
                write!(
                    f,
                    "{}Expr @ {}\n{}Infix @ {}\n{:?}\n{}{:?}\n{:?}",
                    " ".repeat(self.indent),
                    self.value.span,
                    " ".repeat(self.indent + 2),
                    self.value.span,
                    lhs,
                    " ".repeat(self.indent + 4),
                    op,
                    rhs
                )
            }
            Expr::Let { name, expr, body } => {
                let expr = Format {
                    indent: self.indent + 4,
                    value: *expr,
                };
                let body = Format {
                    indent: self.indent + 4,
                    value: *body,
                };
                write!(
                    f,
                    "{}Expr @ {}\n{}Let @ {}\n{}Ident @ {}\n{}{}{:?}\n{:?}",
                    " ".repeat(self.indent),
                    self.value.span,
                    " ".repeat(self.indent + 2),
                    self.value.span,
                    " ".repeat(self.indent + 4),
                    name.span,
                    " ".repeat(self.indent + 6),
                    name.value,
                    expr,
                    body
                )
            }
            Expr::Apply { fun, args } => {
                let fun = Format {
                    indent: self.indent + 4,
                    value: *fun,
                };
                let args = Format {
                    indent: self.indent + 4,
                    value: args.clone(),
                };
                write!(
                    f,
                    "{}Expr @ {}\n{}Apply @ {}\n{:?}\n{:?}",
                    " ".repeat(self.indent),
                    self.value.span,
                    " ".repeat(self.indent + 2),
                    self.value.span,
                    fun,
                    args
                )
            }
            Expr::If {
                cond,
                then,
                elifs,
                else_,
            } => {
                let cond_fmt = Format {
                    indent: self.indent + 4,
                    value: *cond,
                };
                let then_fmt = Format {
                    indent: self.indent + 4,
                    value: *then,
                };
                let elifs_fmt = Format {
                    indent: self.indent + 4,
                    value: elifs
                        .clone()
                        .into_iter()
                        .map(|(c, e)| (*c, *e))
                        .collect::<Vec<_>>(),
                };
                let else_fmt = Format {
                    indent: self.indent + 4,
                    value: *else_,
                };
                if elifs.is_empty() {
                    write!(
                        f,
                        "{}Expr @ {}\n{}If @ {}\n{}Cond\n{:?}\n{}Then\n{:?}\n{}Else\n{:?}",
                        " ".repeat(self.indent),
                        self.value.span,
                        " ".repeat(self.indent + 2),
                        self.value.span,
                        " ".repeat(self.indent + 4),
                        cond_fmt,
                        " ".repeat(self.indent + 4),
                        then_fmt,
                        " ".repeat(self.indent + 4),
                        else_fmt
                    )
                } else {
                    write!(
                    f,
                    "{}Expr @ {}\n{}If @ {}\n{}Cond\n{:?}\n{}Then\n{:?}\n{}Elifs\n{:?}\n{}Else\n{:?}",
                    " ".repeat(self.indent),
                    self.value.span,
                    " ".repeat(self.indent + 2),
                    self.value.span,
                    " ".repeat(self.indent + 4),
                    cond_fmt,
                    " ".repeat(self.indent + 4),
                    then_fmt,
                    " ".repeat(self.indent + 4),
                    elifs_fmt,
                    " ".repeat(self.indent + 4),
                    else_fmt
                )
                }
            }
            Expr::Lambda { params, body } => {
                let params = Format {
                    indent: self.indent + 4,
                    value: params.clone(),
                };
                let body = Format {
                    indent: self.indent + 4,
                    value: *body,
                };
                write!(
                    f,
                    "{}Expr @ {}\n{}Lambda @ {}\n{}Params\n{:?}\n{}Body\n{:?}",
                    " ".repeat(self.indent),
                    self.value.span,
                    " ".repeat(self.indent + 2),
                    self.value.span,
                    " ".repeat(self.indent + 4),
                    params,
                    " ".repeat(self.indent + 4),
                    body
                )
            }
            Expr::Unit => write!(
                f,
                "{}Expr @ {}\n{}Unit",
                " ".repeat(self.indent),
                self.value.span,
                " ".repeat(self.indent + 2),
            ),
            Expr::Error => write!(
                f,
                "{}Expr @ {}\n{}Error",
                " ".repeat(self.indent),
                self.value.span,
                " ".repeat(self.indent + 2)
            ),
        }
    }
}

impl Debug for Format<Vec<(Spanned<Expr>, Spanned<Expr>)>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.value.is_empty() {
            return Ok(());
        }
        for (i, (cond, body)) in self.value.iter().enumerate() {
            let cond = Format {
                indent: self.indent + 2,
                value: cond.clone(),
            };
            let body = Format {
                indent: self.indent + 2,
                value: body.clone(),
            };
            write!(
                f,
                "{}Cond @ {}\n{:?}\n{}Then @ {}\n{:?}",
                " ".repeat(self.indent),
                cond.value.span,
                cond,
                " ".repeat(self.indent),
                body.value.span,
                body
            )?;
            if i != self.value.len() - 1 {
                write!(f, "\n")?;
            }
        }
        Ok(())
    }
}

impl Debug for Format<Vec<Spanned<Expr>>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.value.is_empty() {
            return Ok(());
        }
        for (i, expr) in self.value.iter().enumerate() {
            let expr = Format {
                indent: self.indent + 2,
                value: expr.clone(),
            };
            write!(
                f,
                "{}Expr @ {}\n{:?}",
                " ".repeat(self.indent),
                expr.value.span,
                expr
            )?;
            if i != self.value.len() - 1 {
                write!(f, "\n")?;
            }
        }
        Ok(())
    }
}

impl Debug for Format<Spanned<Lit>> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.value.value {
            Lit::Int(i) => write!(
                f,
                "{}Lit @ {}\n{}Int @ {}\n{}{}",
                " ".repeat(self.indent),
                self.value.span,
                " ".repeat(self.indent + 2),
                self.value.span,
                " ".repeat(self.indent + 4),
                i
            ),
            Lit::Rational(r) => write!(
                f,
                "{}Lit @ {}\n{}Rational @ {}\n{}{}",
                " ".repeat(self.indent),
                self.value.span,
                " ".repeat(self.indent + 2),
                self.value.span,
                " ".repeat(self.indent + 4),
                r
            ),
            Lit::Real(r) => write!(
                f,
                "{}Lit @ {}\n{}Real @ {}\n{}{}",
                " ".repeat(self.indent),
                self.value.span,
                " ".repeat(self.indent + 2),
                self.value.span,
                " ".repeat(self.indent + 4),
                r
            ),
            Lit::Complex(c) => write!(
                f,
                "{}Lit @ {}\n{}Complex @ {}\n{}{}",
                " ".repeat(self.indent),
                self.value.span,
                " ".repeat(self.indent + 2),
                self.value.span,
                " ".repeat(self.indent + 4),
                c
            ),
            Lit::Char(c) => write!(
                f,
                "{}Lit @ {}\n{}Char @ {}\n{}'{}'",
                " ".repeat(self.indent),
                self.value.span,
                " ".repeat(self.indent + 2),
                self.value.span,
                " ".repeat(self.indent + 4),
                c
            ),
            Lit::String(s) => write!(
                f,
                "{}Lit @ {}\n{}String @ {}\n{}{}",
                " ".repeat(self.indent),
                self.value.span,
                " ".repeat(self.indent + 2),
                self.value.span,
                " ".repeat(self.indent + 4),
                s
            ),
        }
    }
}
