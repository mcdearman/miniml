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
        // parse let expr
        let let_ = let_parser(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone().map_with_span(SrcNode::new))
            .map(|((name, expr), body)| Expr::Let { name, expr, body });

        // parse if
        let if_ = just(Token::If)
            .ignore_then(expr.clone().map_with_span(SrcNode::new))
            .then_ignore(just(Token::Then))
            .then(expr.clone().map_with_span(SrcNode::new))
            .then_ignore(just(Token::Else))
            .then(expr.clone().map_with_span(SrcNode::new))
            .map(|((cond, then), else_)| Expr::If {
                cond,
                then,
                elifs: vec![],
                else_,
            })
            .boxed();

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
            .or(if_)
            .or(let_)
            .or(lambda.map(|expr| expr.inner().clone()))
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
            .or(just(Token::Percent)
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

        // cmp = term (("<" | ">" | "<=" | ">=" | "=" | "!=")  cmp)?
        let op = choice((
            just(Token::Lt)
                .map(InfixOp::from)
                .map_with_span(SrcNode::new),
            just(Token::Gt)
                .map(InfixOp::from)
                .map_with_span(SrcNode::new),
            just(Token::Leq)
                .map(InfixOp::from)
                .map_with_span(SrcNode::new),
            just(Token::Geq)
                .map(InfixOp::from)
                .map_with_span(SrcNode::new),
            just(Token::Eq)
                .map(InfixOp::from)
                .map_with_span(SrcNode::new),
            just(Token::Neq)
                .map(InfixOp::from)
                .map_with_span(SrcNode::new),
        ))
        .boxed();

        // cmp = term (("<" | ">" | "<=" | ">=" | "=" | "!=")  term)*
        let cmp = term
            .clone()
            .foldl(
                op.clone().then(term.clone()).repeated(),
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

        cmp.map(|expr| expr.inner().clone())
    })
}

// Helper to reduce boilerplate. Used in both let expressions and let declarations.
fn let_parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
    expr_parser: impl Parser<'a, I, Expr, extra::Err<Rich<'a, Token>>> + 'a,
) -> impl Parser<'a, I, (SrcNode<InternedString>, SrcNode<Expr>), extra::Err<Rich<'a, Token>>> {
    just(Token::Let)
        .ignore_then(ident_parser().map_with_span(SrcNode::new))
        .then(
            ident_parser()
                .map_with_span(SrcNode::new)
                .repeated()
                .collect::<Vec<_>>(),
        )
        .then_ignore(just(Token::Eq))
        .then(expr_parser.map_with_span(SrcNode::new))
        .map(|((name, params), mut body)| {
            for param in params.into_iter().rev() {
                let span = SimpleSpan::new(param.span().start, body.span().end);
                body = SrcNode::new(Expr::Lambda { param, body }, span);
            }
            (name, body)
        })
        .boxed()
}

fn decl_parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Decl, extra::Err<Rich<'a, Token>>> {
    let_parser(expr_parser())
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
