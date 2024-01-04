use crate::{
    ast::{Expr, InfixOp, Lit, MatchCase, Pattern, PrefixOp},
    node::Node,
    span::Span,
    token::Token,
};
use chumsky::{
    extra,
    input::{Stream, ValueInput},
    prelude::{Input, Rich},
    primitive::just,
    recursive::recursive,
    select, IterParser, Parser,
};

pub type ParseError<'a> = Rich<'a, Node<Token>, Span, &'a str>;

fn ident_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl Parser<'a, I, String, extra::Err<Rich<'a, Token, Span>>> {
    select! {
        Token::Ident(name) => name,
    }
}

fn lit_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl Parser<'a, I, Lit, extra::Err<Rich<'a, Token, Span>>> {
    select! {
        Token::Num(n) => Lit::Num(n),
        Token::Bool(b) => Lit::Bool(b),
        Token::String(s) => Lit::String(s),
    }
}
