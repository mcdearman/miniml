use crate::{
    ast::{Decl, Expr, InfixOp, Lit, Root},
    node::{Node, SrcNode},
    token::Token,
};
use chumsky::{
    extra,
    input::ValueInput,
    prelude::{Rich, Simple},
    primitive::{choice, just},
    recursive::recursive,
    select,
    span::SimpleSpan,
    IterParser, Parser,
};
use miniml_util::intern::InternedString;

fn ident_parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
) -> impl Parser<'a, I, InternedString, extra::Err<Rich<'a, Token>>> {
    select! {
        Token::Ident(name) => InternedString::from(name),
    }
}

fn lit_parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Lit, extra::Err<Rich<'a, Token>>> {
    select! {
        Token::Nat(n) => Lit::Nat(n),
        Token::Int(n) => Lit::Int(n),
        Token::Rational(r) => Lit::Rational(r),
        Token::Real(r) => Lit::Real(r),
        Token::Complex(c) => Lit::Complex(c),
        Token::Char(c) => Lit::Char(c),
        Token::String(s) => Lit::String(s),
    }
}

fn expr_parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Expr, extra::Err<Rich<'a, Token>>> {
    recursive(|expr| {
        // parse let
        // let let_ = just(Token::Let)
        //     .ignore_then(ident_parser().map_with_span(SrcNode::new))
        //     .then_ignore(just(Token::Eq))
        //     .then(inline_expr.clone())
        //     .then_ignore(just(Token::In))
        //     .then(expr.clone())
        //     .map(|((name, expr), body)| Expr::Let {
        //         name,
        //         expr,
        //         body,
        //         rec: false,
        //     })
        //     .boxed();

        // parse curry lambda
        let lambda = just(Token::Backslash)
            .ignore_then(
                ident_parser()
                    .map_with_span(SrcNode::new)
                    .repeated()
                    .at_least(1)
                    .foldr(
                        just(Token::Arrow).ignore_then(expr.clone().map_with_span(SrcNode::new)),
                        |param: SrcNode<InternedString>, body: SrcNode<Expr>| {
                            SrcNode::new(
                                Expr::Lambda {
                                    param: param.clone(),
                                    body: body.clone(),
                                },
                                SimpleSpan::new(param.span().start, body.span().end),
                            )
                        },
                    ),
            )
            .boxed();

        let atom = ident_parser()
            .map_with_span(SrcNode::new)
            .map(Expr::Ident)
            .or(lit_parser().map_with_span(SrcNode::new).map(Expr::Lit))
            .or(
                // let_,
                lambda.map(|expr| expr.inner().clone()),
            )
            .or(expr
                .clone()
                .delimited_by(just(Token::LParen), just(Token::RParen)))
            .map_with_span(SrcNode::new)
            .boxed();

        // parse function application
        let apply = atom
            .clone()
            .foldl(atom.clone().repeated(), |fun, arg| {
                SrcNode::new(
                    Expr::Apply {
                        fun: fun.clone(),
                        arg: arg.clone(),
                    },
                    SimpleSpan::new(fun.span().start, arg.span().end),
                )
            })
            .map(|expr| expr.inner().clone())
            .boxed();

        let op = just(Token::Star)
            .map(InfixOp::from)
            .map_with_span(SrcNode::new)
            .or(just(Token::Slash)
                .map(InfixOp::from)
                .map_with_span(SrcNode::new))
            .boxed();

        let factor = apply
            .clone()
            .map_with_span(SrcNode::new)
            .foldl(
                op.clone()
                    .then(apply.clone().map_with_span(SrcNode::new))
                    .repeated(),
                |lhs: SrcNode<Expr>, (op, rhs)| {
                    SrcNode::new(
                        Expr::Infix {
                            op,
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        SimpleSpan::new(lhs.span().start, rhs.span().end),
                    )
                },
            )
            .boxed();

        let op = just(Token::Plus)
            .map(InfixOp::from)
            .map_with_span(SrcNode::new)
            .or(just(Token::Minus)
                .map(InfixOp::from)
                .map_with_span(SrcNode::new))
            .boxed();

        let term = factor
            .clone()
            .foldl(
                op.clone().then(factor.clone()).repeated(),
                |lhs: SrcNode<Expr>, (op, rhs): (_, SrcNode<Expr>)| {
                    SrcNode::new(
                        Expr::Infix {
                            op,
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        SimpleSpan::new(lhs.span().start, rhs.span().end),
                    )
                },
            )
            .boxed();

        term.map(|expr| expr.inner().clone())
    })
}

fn decl_parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Decl, extra::Err<Rich<'a, Token>>> {
    just(Token::Let)
        .ignore_then(ident_parser().map_with_span(SrcNode::new))
        .then_ignore(just(Token::Eq))
        .then(expr_parser().map_with_span(SrcNode::new))
        .map(|(name, expr)| Decl::Let { name, expr })
        .boxed()
}

pub fn parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Root, extra::Err<Rich<'a, Token>>> {
    decl_parser()
        .map_with_span(SrcNode::new)
        .repeated()
        .at_least(1)
        .collect()
        .map(|decls| Root { decls })
        .boxed()
}
