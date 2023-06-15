use chumsky::{extra, input::ValueInput, span::SimpleSpan};

use super::token::Token;

pub trait Parser<'a, T> = chumsky::Parser<'a, T, SimpleSpan, extra::Err<Rich<Token>>>;

pub fn parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>() -> impl Parser {
    todo!()
}
