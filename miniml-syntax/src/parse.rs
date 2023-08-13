use crate::{
    ast::{Expr, Lit, Root},
    lex::Token,
};
use chumsky::{extra, input::ValueInput, prelude::Rich, select, span::SimpleSpan, Parser};
use miniml_util::span::{Spannable, Spanned};

fn expr_parser<'a, I>() -> impl Parser<'a, I, Expr, extra::Err<Rich<'a, Token>>>
where
    I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
    select! {
        Token::Ident(i) => Expr::Ident(i),
        Token::Int(_)
        | Token::Rational
        | Token::Real
        | Token::Complex
        | Token::Char
        | Token::String => Expr::Lit(lit_parser()),
    }
}

fn lit_parser<'a, I>() -> impl Parser<'a, I, Spanned<Lit>, extra::Err<Rich<'a, Token>>>
where
    I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
    select! {
        Token::Int(i) => Lit::Int(i).spanned(),
        Token::Rational(r) => Lit::Rational(r),
        Token::Real(r) => Lit::Real(r),
        Token::Complex(c) => Lit::Complex(c),
        Token::Char(c) => Lit::Char(c),
        Token::String(s) => Lit::String(s),
    }
}
