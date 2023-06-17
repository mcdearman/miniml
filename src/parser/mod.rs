use self::{ast::Item, error::Error, token::Token};
use crate::parser::parse::parser;
use chumsky::{input::Stream, prelude::Input, span::SimpleSpan, Parser};
use logos::Logos;

pub mod ast;
pub mod cst;
pub mod error;
pub mod parse;
pub mod span;
mod tests;
pub mod token;

pub fn parse(src: &str) -> Result<Item, Error> {
    let lex = Token::lexer(src)
        .spanned()
        .map(|(tok, span)| (tok, SimpleSpan::from(span)));
    let tok_stream = Stream::from_iter(lex).spanned(SimpleSpan::from(src.len()..src.len()));
    parser().parse(tok_stream).into_result().map_err(|e| Error {
        span: e.span,
        message: e.value.message,
    })
}
