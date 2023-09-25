use crate::util::span::Span;
use ast::Root;
use chumsky::{
    input::Stream,
    prelude::{Input, Rich},
    span::SimpleSpan,
    Parser,
};
use logos::Logos;
use parse::{parser, repl_parser};
use token::Token;

pub mod ast;
pub mod error;
pub mod parse;
pub mod token;

pub type ParseError<'a> = Rich<'a, token::Token, Span, &'a str>;

pub fn parse<'src>(src: &'src str, repl: bool) -> (Option<Root>, Vec<ParseError<'src>>) {
    let tokens = Token::lexer(&src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, Span::from(span)),
        Err(err) => panic!("lex error: {:?}", err),
    });
    let tok_stream = Stream::from_iter(tokens).spanned(Span::from(src.len()..src.len()));
    if repl {
        repl_parser().parse(tok_stream).into_output_errors()
    } else {
        parser().parse(tok_stream).into_output_errors()
    }
}
