use std::{iter::Peekable, vec::IntoIter};

use crate::T;

use self::{
    ast::Item,
    error::ParserError,
    token::{Token, TokenStream, TokenKind},
};
use itertools::Itertools;
use logos::{Logos, Span};

pub mod ast;
pub mod cst;
pub mod error;
pub mod lex;
pub mod span;
mod tests;
pub mod token;

#[derive(Debug)]
pub struct Parser {
    tokens: TokenStream,
    // errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(tokens: TokenStream) -> Self {
        Self { tokens }
    }

    fn peek(&mut self) -> Token {
        self.tokens.peek()
    }

    fn next(&mut self) -> Token {
        self.tokens.next()
    }

    fn at(&mut self, kind: TokenKind) -> bool {
        self.peek().kind == kind
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.next();
            true
        } else {
            false
        }
    }

    pub fn parse() -> (Option<Item>, Vec<ParserError>) {
        // let mut errors = vec![];
        todo!()
    }

    fn file(&mut self) ->  {
        while !self.eof() {
            self.item()
        }

        self.close(m, SyntaxKind::File);
    }

    fn item(&mut self) {
        match self.peek() {
           T![] 
        }
    }
}
