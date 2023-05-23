use self::ast::*;
use self::cst::Node;
use self::token::*;
use crate::{intern::InternedString, list::List, T};
use logos::{Lexer, Logos};
use num_bigint::BigInt;
use num_complex::Complex64;
use num_rational::Rational64;
use std::fmt::Debug;
use std::{
    collections::HashMap,
    fmt::Display,
    hash::Hash,
    ops::{Index, Range},
};

pub mod cst;
mod tests;
pub mod token;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ParserError(pub String);

impl ParserError {
    pub fn new(msg: &str) -> Self {
        Self(msg.to_string())
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub type Result<T> = std::result::Result<T, ParserError>;

pub struct Parser<'src> {
    src: &'src str,
    logos: Lexer<'src, TokenKind>,
    peek: Option<Token>,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            logos: TokenKind::lexer(src),
            peek: None,
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        self.logos
            .clone()
            .spanned()
            .map(|(t, s)| Token::from((t, s.into())))
            .collect()
    }

    fn text(&self, token: Token) -> &'src str {
        &self.src[token.span]
    }

    fn next(&mut self) -> Token {
        if let Some(t) = self.peek.take() {
            t
        } else {
            self.generate()
        }
    }

    fn peek(&mut self) -> Token {
        if let Some(t) = self.peek.clone() {
            t
        } else {
            let t = self.generate();
            self.peek = Some(t.clone());
            t
        }
    }

    fn generate(&mut self) -> Token {
        match self.logos.next().map(|t| (t, self.logos.span())) {
            None => Token::from((T![EOF], Span::new(0, 0))),
            Some((T![comment], _)) => self.generate(),
            Some((t, s)) => Token::from((t, s.into())),
        }
    }

    fn at(&mut self, kind: TokenKind) -> bool {
        self.peek().value == kind
    }

    fn consume(&mut self, expected: TokenKind) {
        let token = self.next();
        assert_eq!(
            token.value, expected,
            "Expected to consume `{}`, but found `{:?}`",
            expected, token
        );
    }

    fn expr(&mut self) -> Node {
        self.or()
    }

    fn or(&mut self) -> Node {
        let lhs = self.and();
        match self.peek().value {
            T![||] => {
                self.consume(T![||]);
                let rhs = self.and();
            }
            _ => Ok(lhs),
        }
    }

    fn and(&mut self) -> Node {
        todo!()
    }

    // fn curry_fn(&mut self, params: Vec<Pattern>, body: Expr) -> Result<Lambda> {
    //     let last = params.first().ok_or(ParserError(
    //         "Cannot create a lambda with no parameters".to_string(),
    //     ))?;
    //     let iter = params.clone().into_iter().rev().filter(|p| p != last);
    //     let b = iter.fold(body, |acc, p| {
    //         Expr::Lit(Lit::Lambda(Lambda {
    //             param: p,
    //             body: Box::new(acc),
    //         }))
    //     });
    //     Ok(Lambda {
    //         param: last.clone(),
    //         body: Box::new(b),
    //     })
    // }

    // fn curry_apply(&mut self, args: Vec<Expr>, func: Expr) -> Expr {
    //     args.into_iter()
    //         .fold(func, |acc, arg| Expr::Apply(Box::new(acc), Box::new(arg)))
    // }
}
