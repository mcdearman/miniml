use lasso::Spur;
use lasso::ThreadedRodeo;
use logos::Logos;
use once_cell::sync::Lazy;
use std::fmt::{Debug, Display};

// ===============================================================================
// =                               String Interner                               =
// ===============================================================================

pub static mut INTERNER: Lazy<ThreadedRodeo> = Lazy::new(|| ThreadedRodeo::default());

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct InternedString {
    pub key: Spur,
}

impl From<Spur> for InternedString {
    fn from(key: Spur) -> Self {
        Self { key }
    }
}

impl From<&str> for InternedString {
    fn from(name: &str) -> Self {
        Self {
            key: unsafe { INTERNER.get_or_intern(name) },
        }
    }
}

impl Debug for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "InternedString({})", unsafe {
            INTERNER.resolve(&self.key)
        })
    }
}

impl Display for InternedString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", unsafe { INTERNER.resolve(&self.key) })
    }
}

// ===============================================================================
// =                                   Parser                                    =
// ===============================================================================

#[derive(Logos, Debug, Clone, PartialEq, Eq)]
enum Token {
    #[regex(r"\d+", |lex| lex.slice().parse())]
    Int(i64),
    #[regex(r"true|false", |lex| lex.slice().parse())]
    Bool(bool),
    #[regex(r##"([A-Za-z]|_)([A-Za-z]|_|\d)*"##, |lex| InternedString::from(lex.slice()))]
    Ident(InternedString),
    #[token("\\")]
    Lambda,
    #[token("->")]
    Arrow,
    #[token("=")]
    Eq,
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[regex(r"[ \t\n\r]", logos::skip)]
    Whitespace,
    #[error]
    Error,
}

// ===============================================================================
// =                                 Entry Point                                 =
// ===============================================================================

fn main() {
    println!("Hello, world!");
}
