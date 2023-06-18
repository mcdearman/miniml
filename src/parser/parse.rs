use super::{ast::Item, token::Token};
use crate::{
    list::List,
    parser::ast::{Expr, Lit},
};
use chumsky::{
    extra, input::ValueInput, prelude::Rich, primitive::just, recursive::recursive, select,
    span::SimpleSpan, IterParser, Parser,
};

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
        Token::Rational(n) => Lit::Rational(n),
        Token::Real(n) => Lit::Real(n),
        Token::Imag(n) => Lit::Complex(n),
        Token::Char(c) => Lit::Char(c),
        Token::String(s) => Lit::String(s),
        Token::Bool(b) => Lit::Bool(b),
        // Token::LBrack => list_parser(),
        // Token::LParen => todo!(),
        // Token::LBrace => todo!(),
    }
}

fn list_parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Lit, extra::Err<Rich<'a, Token>>> + Clone {
    recursive(|list| {
        let exprs = expr_parser()
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<Expr>>()
            .delimited_by(just(Token::LBrack), just(Token::RBrack).ignored())
            .boxed()
            .map(Lit::List);
        exprs
    })
}
