use std::borrow::Borrow;

use super::{ast::Item, token::Token};
use crate::{
    list::List,
    parser::ast::{Expr, Lit},
};
use chumsky::{
    extra,
    input::ValueInput,
    prelude::Rich,
    primitive::{choice, just},
    recursive::recursive,
    select,
    span::SimpleSpan,
    IterParser, Parser,
};

// pub trait Parser<'a, T> = chumsky::Parser<'a,  T, SimpleSpan, extra::Err<Rich<Token>> + Clone;

pub fn parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Item, extra::Err<Rich<'a, Token>>> + Clone {
    expr_parser().map(Item::Expr)
}

fn expr_parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Expr, extra::Err<Rich<'a, Token>>> + Clone {
    let ident = select! { Token::Ident(name) => name };
    recursive(|expr| {
        let lit = select! {
            Token::Int(n) => Lit::Int(n),
            Token::Rational(n) => Lit::Rational(n),
            Token::Real(n) => Lit::Real(n),
            Token::Imag(n) => Lit::Complex(n),
            Token::Char(c) => Lit::Char(c),
            Token::String(s) => Lit::String(s),
            Token::Bool(b) => Lit::Bool(b),
        };

        let list = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect::<Vec<Expr>>()
            .delimited_by(just(Token::LBrack), just(Token::RBrack).ignored())
            .boxed()
            .map(Expr::List);

        let elif = just(Token::Elif)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .map(|(cond, then)| (cond, then))
            .boxed();

        let if_ = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .then(elif.repeated())
            .foldr(expr.clone(), f)
            .then_ignore(just(Token::Else))
            .then(expr.clone())
            .map(|((cond, then), else_)| Expr::If {
                cond: Box::new(cond),
                then: Box::new(then),
                else_: Box::new(else_),
            })
            .boxed();

        // <atom> ::= <ident> | <lit> | <list> | <if> | <letExpr> | <fnExpr> | "(" <expr> ")"
        let atom = choice((
            ident.map(Expr::Ident),
            lit.map(Expr::Lit),
            list,
            if_,
            expr.delimited_by(just(Token::LParen), just(Token::RParen)),
        ));

        atom
    })
}
