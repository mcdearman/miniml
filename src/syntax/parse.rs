use super::{
    ast::{Expr, InfixOp, Item, Lit, PrefixOp, Root},
    token::Token,
};
use crate::util::{intern::InternedString, node::SrcNode, span::Span};
use chumsky::{
    extra,
    input::{Stream, ValueInput},
    prelude::{Input, Rich},
    primitive::just,
    recursive::recursive,
    select, IterParser, Parser,
};
use logos::Logos;

pub type ParseError<'a> = Rich<'a, Token, Span, &'a str>;

fn ident_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl Parser<'a, I, InternedString, extra::Err<Rich<'a, Token, Span>>> {
    select! {
        Token::Ident(name) => name,
    }
}

fn lit_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl Parser<'a, I, Lit, extra::Err<Rich<'a, Token, Span>>> {
    select! {
        Token::Num(n) => Lit::Num(n),
        Token::Bool(b) => Lit::Bool(b),
    }
}

fn expr_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl Parser<'a, I, Expr, extra::Err<Rich<'a, Token, Span>>> {
    recursive(|expr| {
        let unit = just(Token::LParen)
            .ignore_then(just(Token::RParen))
            .map(|_| Expr::Unit);

        let lit = lit_parser().map(Expr::Lit);

        let let_ = just(Token::Let)
            .ignore_then(
                ident_parser()
                    .map_with_span(SrcNode::new)
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::Eq))
            .then(expr.clone().map_with_span(SrcNode::new))
            .then_ignore(just(Token::In))
            .then(expr.clone().map_with_span(SrcNode::new))
            .map(|((names, expr), body)| {
                if names.len() == 1 {
                    Expr::Let {
                        name: names[0].clone(),
                        expr,
                        body,
                    }
                } else {
                    let expr = SrcNode::new(
                        Expr::Lambda {
                            params: names[1..].to_vec(),
                            body: expr.clone(),
                        },
                        Span::new(names[0].span().start, expr.span().end),
                    );
                    Expr::Let {
                        name: names[0].clone(),
                        expr,
                        body,
                    }
                }
            });

        let lambda = just(Token::Lambda)
            .ignore_then(
                ident_parser()
                    .map_with_span(SrcNode::new)
                    .repeated()
                    .at_least(1)
                    .collect::<Vec<_>>(),
            )
            .then_ignore(just(Token::Arrow))
            .then(expr.clone().map_with_span(SrcNode::new))
            .map(|(params, body)| Expr::Lambda { params, body });

        let if_ = just(Token::If)
            .ignore_then(expr.clone().map_with_span(SrcNode::new))
            .then_ignore(just(Token::Then))
            .then(expr.clone().map_with_span(SrcNode::new))
            .then_ignore(just(Token::Else))
            .then(expr.clone().map_with_span(SrcNode::new))
            .map(|((cond, then), else_)| Expr::If { cond, then, else_ });

        // atom = ident | number | bool | '(' expr ')'
        let atom = ident_parser()
            .map(Expr::Ident)
            .or(unit)
            .or(lit)
            .or(let_)
            .or(if_)
            .or(lambda)
            .or(expr
                .clone()
                .delimited_by(just(Token::LParen), just(Token::RParen)))
            .map_with_span(SrcNode::new)
            .boxed();

        // apply = atom+
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
            .or(just(Token::Bang)
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

        term.map(|expr| expr.inner().clone())
        // atom.clone()
    })
}

fn def_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl Parser<'a, I, SrcNode<Item>, extra::Err<Rich<'a, Token, Span>>> {
    ident_parser()
        .map_with_span(SrcNode::new)
        .repeated()
        .at_least(1)
        .collect::<Vec<_>>()
        .then_ignore(just(Token::Eq))
        .then(expr_parser().map_with_span(SrcNode::new))
        .map(|(names, expr)| {
            if names.len() == 1 {
                Item::Def {
                    name: names[0].clone(),
                    expr,
                }
            } else {
                let expr = SrcNode::new(
                    Expr::Lambda {
                        params: names[1..].to_vec(),
                        body: expr.clone(),
                    },
                    Span::new(names[0].span().start, expr.span().end),
                );
                Item::Def {
                    name: names[0].clone(),
                    expr,
                }
            }
        })
        .map_with_span(SrcNode::new)
        .boxed()
}

fn item_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl Parser<'a, I, SrcNode<Item>, extra::Err<Rich<'a, Token, Span>>> {
    def_parser()
        .or(expr_parser().map(Item::Expr).map_with_span(SrcNode::new))
        .boxed()
}

fn parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl Parser<'a, I, SrcNode<Root>, extra::Err<Rich<'a, Token, Span>>> {
    item_parser()
        .repeated()
        .collect()
        .map(|items| Root { items })
        .map_with_span(SrcNode::new)
        .boxed()
}

pub fn parse<'src>(src: &'src str) -> (Option<SrcNode<Root>>, Vec<ParseError<'src>>) {
    let tokens = Token::lexer(&src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, Span::from(span)),
        Err(err) => panic!("lex error: {:?}", err),
    });
    let tok_stream = Stream::from_iter(tokens).spanned(Span::from(src.len()..src.len()));
    parser().parse(tok_stream).into_output_errors()
}

mod tests {
    use crate::syntax::parse::parse;

    #[test]
    fn parse_unit() {
        let (root, errs) = parse("()");
        if !errs.is_empty() {
            panic!("parse error: {:?}", errs);
        }
        insta::assert_debug_snapshot!(root.unwrap());
    }

    #[test]
    fn parse_num() {
        let (root, errs) = parse("1");
        if !errs.is_empty() {
            panic!("parse error: {:?}", errs);
        }
        insta::assert_debug_snapshot!(root.unwrap());
    }

    #[test]
    fn parse_bool() {
        let (root, errs) = parse("true");
        if !errs.is_empty() {
            panic!("parse error: {:?}", errs);
        }
        insta::assert_debug_snapshot!(root.unwrap());
    }

    #[test]
    fn parse_ident() {
        let (root, errs) = parse("x");
        if !errs.is_empty() {
            panic!("parse error: {:?}", errs);
        }
        insta::assert_debug_snapshot!(root.unwrap());
    }

    #[test]
    fn parse_let() {
        let (root, errs) = parse("let x = 1 in x");
        if !errs.is_empty() {
            panic!("parse error: {:?}", errs);
        }
        insta::assert_debug_snapshot!(root.unwrap());
    }

    #[test]
    fn parse_let_fn() {
        let (root, errs) = parse("let add x y = x + y in add 1 2");
        if !errs.is_empty() {
            panic!("parse error: {:?}", errs);
        }
        insta::assert_debug_snapshot!(root.unwrap());
    }

    #[test]
    fn parse_apply() {
        let (root, errs) = parse("f x y");
        if !errs.is_empty() {
            panic!("parse error: {:?}", errs);
        }
        insta::assert_debug_snapshot!(root.unwrap());
    }

    #[test]
    fn parse_prefix() {
        let (root, errs) = parse("-x");
        if !errs.is_empty() {
            panic!("parse error: {:?}", errs);
        }
        insta::assert_debug_snapshot!(root.unwrap());
    }

    #[test]
    fn parse_infix() {
        let (root, errs) = parse("x + y * z");
        if !errs.is_empty() {
            panic!("parse error: {:?}", errs);
        }
        insta::assert_debug_snapshot!(root.unwrap());
    }

    #[test]
    fn parse_paren() {
        let (root, errs) = parse("(x + y) * z");
        if !errs.is_empty() {
            panic!("parse error: {:?}", errs);
        }
        insta::assert_debug_snapshot!(root.unwrap());
    }

    #[test]
    fn parse_lambda() {
        let (root, errs) = parse("\\a b -> a + b");
        if !errs.is_empty() {
            panic!("parse error: {:?}", errs);
        }
        insta::assert_debug_snapshot!(root.unwrap());
    }

    #[test]
    fn parse_lambda_apply() {
        let (root, errs) = parse("(\\a b -> a + b) 1 2");
        if !errs.is_empty() {
            panic!("parse error: {:?}", errs);
        }
        insta::assert_debug_snapshot!(root.unwrap());
    }

    #[test]
    fn parse_def() {
        let (root, errs) = parse("x = 1");
        if !errs.is_empty() {
            panic!("parse error: {:?}", errs);
        }
        insta::assert_debug_snapshot!(root.unwrap());
    }

    #[test]
    fn parse_fn_def() {
        let (root, errs) = parse("add x y = x + y");
        if !errs.is_empty() {
            panic!("parse error: {:?}", errs);
        }
        insta::assert_debug_snapshot!(root.unwrap());
    }
}
