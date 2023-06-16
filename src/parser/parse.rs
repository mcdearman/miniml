use chumsky::{extra, input::ValueInput, prelude::Rich, select, span::SimpleSpan, Parser};

use crate::parser::token::TokenKind;

use super::{cst::Tree, token::Token};

// pub trait Parser<'a, T> = chumsky::Parser<'a, T, SimpleSpan, extra::Err<Rich<Token>>>;

pub fn parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Tree, extra::Err<Rich<'a, Token>>> {
    lit_parser()
}

fn lit_parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Tree, extra::Err<Rich<'a, Token>>> {
    select! {
        Token {
            kind: TokenKind::Int,
            span,
        } => {
            let value = 
            Tree::Int(value)
        }
    }
}
