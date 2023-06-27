use std::iter::Peekable;

use crate::T;

use self::{
    ast::{File, Item},
    error::Error,
    span::Span,
    token::{Token, TokenKind, TokenStream},
};
use itertools::Itertools;
use logos::{Lexer, Logos};

pub mod ast;
pub mod cst;
pub mod error;
pub mod span;
mod tests;
pub mod token;

#[derive(Debug)]
pub struct Parser<'src> {
    lexer: Lexer<'src, TokenKind>,
    peek: Option<Token>,
    src: &'src str,
    errors: Vec<Error>,
}

impl<'src> Parser<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            lexer: TokenKind::lexer(src),
            peek: None,
            src,
            errors: vec![],
        }
    }

    fn fetch_token(&mut self) -> Token {
        match self.lexer.next().map(|res| (res, self.lexer.span())) {
            None => Token {
                kind: T![EOF],
                span: Span::new(0, 0),
            },
            Some((Ok(t), s)) => match t {
                T![comment] => self.fetch_token(),
                _ => Token {
                    kind: t,
                    span: s.into(),
                },
            },
            Some((Err(e), s)) => {
                self.errors.push(Error {
                    message: format!("Lexer error: {:?}: {:?}", s, e),
                });
                self.fetch_token()
            }
        }
    }

    fn next(&mut self) -> Token {
        if let Some(t) = self.peek.take() {
            t
        } else {
            self.fetch_token()
        }
    }

    fn peek(&mut self) -> Token {
        if let Some(t) = self.peek.clone() {
            t
        } else {
            let t = self.fetch_token();
            self.peek = Some(t.clone());
            t
        }
    }

    fn text(&self, token: Token) -> &'src str {
        &self.src[token.span]
    }

    pub fn parse(&mut self) -> (Option<File>, Vec<Error>) {
        let file = self.file();
        (Some(file), self.errors.clone())
    }

    fn file(&mut self) -> File {
        let mut items = vec![];
        while self.peek().kind != T![EOF] {
            items.push(self.item());
        }
        File { items }
    }

    fn item(&mut self) -> Item {
        todo!()
    }
}
