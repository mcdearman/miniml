use self::{ast::Item, error::Error, token::Token};
use crate::parser::parse::parser;
use chumsky::{input::Stream, prelude::Input, span::SimpleSpan, Parser};
use itertools::Itertools;
use logos::{Logos, Span};

pub mod ast;
pub mod cst;
pub mod error;
pub mod lex;
pub mod parse;
pub mod span;
mod tests;
pub mod token;

pub fn parse(src: &str) -> (Option<Item>, Vec<Error>) {
    let mut errors = vec![];

    let (tokens, lex_errs) = lex::lex(src);
    errors.append(
        &mut lex_errs
            .into_iter()
            .map(|e| Error {
                message: format!("{}", e),
            })
            .collect_vec(),
    );

    let tok_stream =
        Stream::from_iter(tokens.into_iter()).spanned(SimpleSpan::from(src.len()..src.len()));

    let res = parser().parse(tok_stream).into_result().map_err(|errs| {
        errs.into_iter().map(|e| Error {
            message: format!("{:?}", e),
        })
    });

    match res {
        Ok(item) => (Some(item), errors),
        Err(errs) => {
            errors.append(&mut errs.collect_vec());
            (None, errors)
        }
    }
}
