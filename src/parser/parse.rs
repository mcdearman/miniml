use chumsky::{
    extra, input::ValueInput, prelude::Rich, recursive::recursive, select, span::SimpleSpan, Parser,
};

use crate::parser::ast::{Expr, Lit};

use super::{ast::Item, cst::Tree, token::Token};

// pub trait Parser<'a, T> = chumsky::Parser<'a, T, SimpleSpan, extra::Err<Rich<Token>>>;
// pub trait Parser<'a, T> = chumsky::Parser<'a,  T, SimpleSpan, extra::Err<Rich<Token>> + Clone;

pub fn parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Item, extra::Err<Rich<'a, Token>>> + Clone {
    recursive(|item| {
        let expr = expr_parser().map(Item::Expr);
        expr
    })
}

fn expr_parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Expr, extra::Err<Rich<'a, Token>>> + Clone {
    recursive(|expr| {
        let lit = lit_parser().map(Expr::Lit);
        lit
    })
}

fn lit_parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Lit, extra::Err<Rich<'a, Token>>> + Clone {
    select! {
        Token::Int(n) => Lit::Int(n),
        Token::Real(n) => Lit::Real(n),
        Token::Imag(n) => Lit::Complex(n),
        Token::Char(c) => Lit::Char(c),
        Token::String(s) => Lit::String(s),
        Token::Bool(b) => Lit::Bool(b),
    }
}
