use super::{
    ast::{Decl, Expr, InfixOp, Lit, MatchCase, Pattern, PrefixOp, Root},
    token::Token,
};
use crate::util::{intern::InternedString, node::SrcNode, span::Span};
use chumsky::{
    extra,
    input::ValueInput,
    prelude::Rich,
    primitive::{choice, just},
    recursive::recursive,
    select, IterParser, Parser,
};

fn ident_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl Parser<'a, I, InternedString, extra::Err<Rich<'a, Token, Span>>> {
    select! {
        Token::Ident(name) => InternedString::from(name),
    }
}

fn lit_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl Parser<'a, I, Lit, extra::Err<Rich<'a, Token, Span>>> {
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

fn pattern_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl Parser<'a, I, Pattern, extra::Err<Rich<'a, Token, Span>>> {
    ident_parser()
        .map_with_span(SrcNode::new)
        .map(Pattern::Ident)
        .or(lit_parser().map_with_span(SrcNode::new).map(Pattern::Lit))
        .boxed()
}

fn expr_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl Parser<'a, I, Expr, extra::Err<Rich<'a, Token, Span>>> {
    recursive(|expr| {
        // case = pat "->" expr
        let case = pattern_parser()
            .map_with_span(SrcNode::new)
            .then_ignore(just(Token::Arrow))
            .then(expr.clone().map_with_span(SrcNode::new))
            .map(|(pattern, body): (SrcNode<Pattern>, SrcNode<Expr>)| {
                SrcNode::new(
                    MatchCase {
                        pattern: pattern.clone(),
                        body: body.clone(),
                    },
                    Span::new(pattern.span().start, body.span().end),
                )
            })
            .boxed();

        // match = "match" expr "with" ("|" case)* "\\" case
        let match_ = just(Token::Match)
            .ignore_then(expr.clone().map_with_span(SrcNode::new))
            .then_ignore(just(Token::With))
            .then(
                just(Token::Pipe)
                    .ignore_then(case.clone())
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::Backslash))
            .then(case.clone())
            .map(
                |((expr, mut cases), end): ((_, Vec<SrcNode<MatchCase>>), SrcNode<MatchCase>)| {
                    cases.push(end);
                    Expr::Match { expr, cases }
                },
            );

        // parse let expr
        let let_ = let_parser(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone().map_with_span(SrcNode::new))
            .map(|((name, params, expr), body)| {
                if params.is_empty() {
                    Expr::Let { name, expr, body }
                } else {
                    Expr::Fn {
                        name,
                        params,
                        expr,
                        body,
                    }
                }
            });

        // parse if
        let if_ = just(Token::If)
            .ignore_then(expr.clone().map_with_span(SrcNode::new))
            .then_ignore(just(Token::Then))
            .then(expr.clone().map_with_span(SrcNode::new))
            .then(
                just(Token::Elif)
                    .ignore_then(expr.clone().map_with_span(SrcNode::new))
                    .then_ignore(just(Token::Then))
                    .then(expr.clone().map_with_span(SrcNode::new))
                    .repeated()
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::Else))
            .then(expr.clone().map_with_span(SrcNode::new))
            .map(|(((cond, then), elifs), else_)| Expr::If {
                cond,
                then,
                elifs,
                else_,
            })
            .boxed();

        let lambda = just(Token::Fn)
            .ignore_then(
                ident_parser()
                    .map_with_span(SrcNode::new)
                    .repeated()
                    .at_least(1)
                    .collect(),
            )
            .then(just(Token::Arrow).ignore_then(expr.clone().map_with_span(SrcNode::new)))
            .map(|(params, body)| Expr::Lambda { params, body })
            .map_with_span(SrcNode::new)
            .boxed();

        let atom = ident_parser()
            .map_with_span(SrcNode::new)
            .map(Expr::Ident)
            .or(lit_parser().map_with_span(SrcNode::new).map(Expr::Lit))
            .or(if_)
            .or(match_)
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
            .then(atom.clone().repeated().collect::<Vec<_>>())
            .map(|(fun, args)| {
                if args.is_empty() {
                    fun.clone()
                } else {
                    SrcNode::new(
                        Expr::Apply {
                            fun: fun.clone(),
                            args: args.clone(),
                        },
                        Span::new(fun.span().start, args.last().unwrap().span().end),
                    )
                }
            })
            .boxed();

        let op = just(Token::Minus)
            .map(PrefixOp::from)
            .map_with_span(SrcNode::new)
            .or(just(Token::Not)
                .map(PrefixOp::from)
                .map_with_span(SrcNode::new))
            .boxed();

        // unary = ("-" | "not")* apply
        let unary = op
            .clone()
            .repeated()
            .foldr(
                apply.clone(),
                |op: SrcNode<PrefixOp>, expr: SrcNode<Expr>| {
                    SrcNode::new(
                        Expr::Prefix {
                            op: op.clone(),
                            expr: expr.clone(),
                        },
                        Span::new(op.span().start, expr.span().end),
                    )
                },
            )
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

        let factor = unary
            .clone()
            .foldl(
                op.clone().then(unary.clone()).repeated(),
                |lhs: SrcNode<Expr>, (op, rhs): (SrcNode<InfixOp>, SrcNode<Expr>)| {
                    SrcNode::new(
                        Expr::Infix {
                            op,
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        Span::new(lhs.span().start, rhs.span().end),
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
                        Span::new(lhs.span().start, rhs.span().end),
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
                        Span::new(lhs.span().start, rhs.span().end),
                    )
                },
            )
            .boxed();

        // and = cmp ("and" cmp)*
        let and = cmp
            .clone()
            .foldl(
                just(Token::And).ignore_then(cmp.clone()).repeated(),
                |lhs: SrcNode<Expr>, rhs: SrcNode<Expr>| {
                    SrcNode::new(
                        Expr::Infix {
                            op: SrcNode::new(InfixOp::And, Span::new(0, 0)),
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        Span::new(lhs.span().start, rhs.span().end),
                    )
                },
            )
            .boxed();

        // or = and ("or" and)*
        let or = and
            .clone()
            .foldl(
                just(Token::Or).ignore_then(and.clone()).repeated(),
                |lhs: SrcNode<Expr>, rhs: SrcNode<Expr>| {
                    SrcNode::new(
                        Expr::Infix {
                            op: SrcNode::new(InfixOp::Or, Span::new(0, 0)),
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        Span::new(lhs.span().start, rhs.span().end),
                    )
                },
            )
            .boxed();

        // stmt = or (";" or)*
        let stmt = or
            .clone()
            .foldl(
                just(Token::Semicolon).ignore_then(or.clone()).repeated(),
                |lhs: SrcNode<Expr>, rhs: SrcNode<Expr>| {
                    SrcNode::new(
                        Expr::Infix {
                            op: SrcNode::new(InfixOp::Stmt, Span::new(0, 0)),
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        Span::new(lhs.span().start, rhs.span().end),
                    )
                },
            )
            .boxed();

        // pipe = stmt ("|>" stmt)*
        let pipe = stmt
            .clone()
            .foldl(
                just(Token::PipeArrow).ignore_then(stmt.clone()).repeated(),
                |lhs: SrcNode<Expr>, rhs: SrcNode<Expr>| {
                    SrcNode::new(
                        Expr::Infix {
                            op: SrcNode::new(InfixOp::Pipe, Span::new(0, 0)),
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        Span::new(lhs.span().start, rhs.span().end),
                    )
                },
            )
            .boxed();

        pipe.map(|expr| expr.inner().clone())
    })
}

// Helper to reduce boilerplate. Used in both let expressions and let declarations.
fn let_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
    expr_parser: impl Parser<'a, I, Expr, extra::Err<Rich<'a, Token, Span>>> + 'a,
) -> impl Parser<
    'a,
    I,
    (
        SrcNode<InternedString>,
        Vec<SrcNode<InternedString>>,
        SrcNode<Expr>,
    ),
    extra::Err<Rich<'a, Token, Span>>,
> {
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
        .map(|((name, params), body)| (name, params, body))
        .boxed()
}

fn decl_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl Parser<'a, I, Decl, extra::Err<Rich<'a, Token, Span>>> {
    let_parser(expr_parser())
        .map(|(name, params, expr)| {
            if params.is_empty() {
                Decl::Let { name, expr }
            } else {
                Decl::Fn { name, params, expr }
            }
        })
        .boxed()
}

pub fn parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl Parser<'a, I, Root, extra::Err<Rich<'a, Token, Span>>> {
    decl_parser()
        .map_with_span(SrcNode::new)
        .repeated()
        // .at_least(1)
        .collect()
        .map(|decls| Root { decls })
        .boxed()
}

pub fn repl_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl Parser<'a, I, Root, extra::Err<Rich<'a, Token, Span>>> {
    decl_parser()
        .or(expr_parser()
            .map_with_span(SrcNode::new)
            .map(|expr| Decl::Let {
                name: SrcNode::new(InternedString::from("main"), expr.span()),
                expr,
            }))
        .map_with_span(SrcNode::new)
        .repeated()
        .at_least(1)
        .collect()
        .map(|decls| Root { decls })
        .boxed()
}
