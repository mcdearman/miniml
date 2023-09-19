use miniml_util::intern::InternedString;
use num_complex::Complex64;
use num_rational::Rational64;

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Let { name: InternedString, expr: Expr },
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Ident(InternedString),
    Lit(Lit),
    Lambda {
        param: Box<Self>,
        body: Box<Self>,
    },
    Match {
        expr: Box<Self>,
        cases: Vec<MatchCase>,
    },
    Apply {
        fun: Box<Self>,
        arg: Box<Self>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Ident(InternedString),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Nat(u64),
    Int(i64),
    Rational(Rational64),
    Real(f64),
    Complex(Complex64),
    String(InternedString),
}
