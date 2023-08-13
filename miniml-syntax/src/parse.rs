use crate::{
    ast::{Expr, InfixOp, Lit, PrefixOp, Root, Spanned},
    lex::Token,
};
use chumsky::{
    extra,
    input::ValueInput,
    prelude::Rich,
    primitive::{choice, just},
    recursive, select,
    span::SimpleSpan,
    Parser,
};

fn expr_parser<'a, I>() -> impl Parser<'a, I, Expr, extra::Err<Rich<'a, Token>>>
where
    I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
    recursive(|expr| {
        let ident = select! { Token::Ident(name) => Expr::Ident };
        let lit = lit_parser().map(Expr::Lit);

        let atom  = ident.or(lit);

        let op = just(Token::Plus)
            .to(InfixOp::Add)
            .or(just(Token::Minus).to(InfixOp::Sub));

        let term = atom
            .map(|(lhs, (op, rhs))| Expr::Infix { op, lhs, rhs })
    })
}

fn lit_parser<'a, I>() -> impl Parser<'a, I, Spanned<Lit>, extra::Err<Rich<'a, Token>>>
where
    I: ValueInput<'a, Token = Token, Span = SimpleSpan>,
{
    select! {
        Token::Int(i) => Lit::Int(i),
        Token::Rational(r) => Lit::Rational(r),
        Token::Real(r) => Lit::Real(r),
        Token::Complex(c) => Lit::Complex(c),
        Token::Char(c) => Lit::Char(c),
        Token::String(s) => Lit::String(s),
    }
    .map_with_span(|l, s| (l, s))
}
