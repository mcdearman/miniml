use crate::{
    ast::{Expr, Lit, Root, Spanned},
    lex::Token,
};
use chumsky::{extra, input::ValueInput, prelude::Rich, select, span::SimpleSpan, Parser, recursive};

fn expr_parser<'a, I>() -> impl Parser<'a, I, Expr, extra::Err<Rich<'a, Token>>>
where
    I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{

    recursive(|expr| {
        let ident = select! { Token::Ident(name) => Expr::Ident };

        let lit = select! {
        Token::Int(i) => Lit::Int(i),
        Token::Rational(r) => Lit::Rational(r),
        Token::Real(r) => Lit::Real(r),
        Token::Complex(c) => Lit::Complex(c),
        Token::Char(c) => Lit::Char(c),
        Token::String(s) => Lit::String(s),
        }
    })
}

// fn lit_parser<'a, I>() -> impl Parser<'a, I, Spanned<Lit>, extra::Err<Rich<'a, Token>>>
// where
//     I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
// {
//     select! {
//         Token::Int(i) => Lit::Int(i),
//         Token::Rational(r) => Lit::Rational(r),
//         Token::Real(r) => Lit::Real(r),
//         Token::Complex(c) => Lit::Complex(c),
//         Token::Char(c) => Lit::Char(c),
//         Token::String(s) => Lit::String(s),
//     }.map_with_span(|l, s| (l, s))
// }
