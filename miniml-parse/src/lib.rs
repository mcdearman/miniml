use self::ast::{
    binary_op::BinaryOp, binary_op_kind::BinaryOpKind, decl::Decl, decl_kind::DeclKind, expr::Expr,
    expr_kind::ExprKind, ident::Ident, lit::Lit, root::Root, unary_op::UnaryOp,
    unary_op_kind::UnaryOpKind,
};
use ast::{
    pattern::{self, Pattern},
    pattern_kind::PatternKind,
};
use chumsky::{
    error::Rich,
    extra,
    input::{Input, Stream, ValueInput},
    primitive::just,
    recursive::recursive,
    select, IterParser, Parser as ChumskyParser,
};
use logos::Logos;
use miniml_lex::token::Token;
use miniml_utils::{interned_string::InternedString, span::Span};

pub mod ast;

pub fn parse<'src>(src: &'src str, repl: bool) -> (Option<Root>, Vec<Rich<'src, Token, Span>>) {
    let tokens = Token::lexer(&src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, Span::from(span)),
        Err(_) => (Token::Error, Span::from(span)),
    });
    let tok_stream = Stream::from_iter(tokens).spanned(Span::from(src.len()..src.len()));
    if repl {
        repl_parser().parse(tok_stream).into_output_errors()
    } else {
        root_parser().parse(tok_stream).into_output_errors()
    }
}

fn root_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Root, extra::Err<Rich<'a, Token, Span>>> {
    decl_parser()
        .repeated()
        .collect()
        .map_with_span(Root::new)
        .boxed()
}

fn repl_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Root, extra::Err<Rich<'a, Token, Span>>> {
    expr_parser()
        .map(|e| DeclKind::Let {
            pattern: Pattern::new(
                PatternKind::Ident(Ident::new(InternedString::from("main"), e.span().clone())),
                e.span().clone(),
            ),
            expr: Expr::new(
                ExprKind::Lambda {
                    param: Pattern::new(
                        PatternKind::Ident(Ident::new(
                            InternedString::from("args"),
                            e.span().clone(),
                        )),
                        e.span().clone(),
                    ),
                    expr: e.clone(),
                },
                e.span().clone(),
            ),
        })
        .map_with_span(Decl::new)
        .or(decl_parser())
        .map_with_span(|decl, span| Root::new(vec![decl], span))
        .boxed()
}

fn decl_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Decl, extra::Err<Rich<'a, Token, Span>>> {
    let let_ = just(Token::Let)
        .ignore_then(pattern_parser())
        .then_ignore(just(Token::Assign))
        .then(expr_parser())
        .map(|(pattern, expr)| DeclKind::Let { pattern, expr });

    let fn_ = just(Token::Let)
        .ignore_then(ident_parser())
        .then(pattern_parser().repeated().at_least(1).collect())
        .then_ignore(just(Token::Assign))
        .then(expr_parser())
        .map(|((name, params), expr)| DeclKind::Let {
            pattern: Pattern::new(
                PatternKind::Ident(name.clone()),
                name.span().extend(*expr.span()),
            ),
            expr: curry_fn(params, expr),
        });

    let_.or(fn_).map_with_span(Decl::new).boxed()
}

fn expr_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Expr, extra::Err<Rich<'a, Token, Span>>> {
    recursive(|expr| {
        let unit = just(Token::LParen)
            .ignore_then(just(Token::RParen))
            .map(|_| ExprKind::Unit)
            .map_with_span(Expr::new);

        let lit = lit_parser().map(ExprKind::Lit).map_with_span(Expr::new);

        let let_ = just(Token::Let)
            .ignore_then(pattern_parser())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((pattern, expr), body)| ExprKind::Let {
                pattern,
                expr,
                body,
            })
            .map_with_span(Expr::new);

        let fn_ = just(Token::Let)
            .ignore_then(pattern_parser())
            .then(pattern_parser().repeated().at_least(1).collect())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|(((pattern, params), expr), body)| ExprKind::Let {
                pattern,
                expr: curry_fn(params, expr),
                body,
            })
            .map_with_span(Expr::new);

        let lambda = just(Token::Backslash)
            .ignore_then(pattern_parser().repeated().at_least(1).collect())
            .then_ignore(just(Token::RArrow))
            .then(expr.clone())
            .map(|(params, expr)| curry_fn(params, expr));

        let if_ = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .then_ignore(just(Token::Else))
            .then(expr.clone())
            .map(|((cond, then), else_)| ExprKind::If { cond, then, else_ })
            .map_with_span(Expr::new);

        let atom = ident_parser()
            .map(ExprKind::Ident)
            .map_with_span(Expr::new)
            .or(unit)
            .or(lit)
            .or(let_)
            .or(fn_)
            .or(lambda)
            .or(if_)
            .or(expr
                .clone()
                .delimited_by(just(Token::LParen), just(Token::RParen)))
            .boxed();

        let apply = atom
            .clone()
            .then(atom.clone().repeated().collect::<Vec<_>>())
            .map(|(fun, args)| {
                if args.len() == 0 {
                    fun.clone()
                } else {
                    args.into_iter().fold(fun, |fun, arg| {
                        Expr::new(
                            ExprKind::Apply {
                                fun: fun.clone(),
                                arg: arg.clone(),
                            },
                            fun.span().extend(*arg.span()),
                        )
                    })
                }
            })
            .boxed();

        let op = just(Token::Minus)
            .map(UnaryOpKind::from)
            .or(just(Token::Bang).map(UnaryOpKind::from))
            .map_with_span(UnaryOp::new)
            .boxed();

        let unary = op
            .clone()
            .repeated()
            .foldr(apply.clone(), |op, expr| {
                Expr::new(
                    ExprKind::Unary {
                        op: op.clone(),
                        expr: expr.clone(),
                    },
                    op.span().extend(*expr.span()),
                )
            })
            .boxed();

        let op = just(Token::Caret)
            .map(BinaryOpKind::from)
            .map_with_span(BinaryOp::new)
            .boxed();

        let pow = unary
            .clone()
            .foldl(
                op.clone().then(unary.clone()).repeated(),
                |lhs: Expr, (op, rhs): (BinaryOp, Expr)| {
                    Expr::new(
                        ExprKind::Binary {
                            op: op.clone(),
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        lhs.span().extend(*rhs.span()),
                    )
                },
            )
            .boxed();

        let op = just(Token::Star)
            .map(BinaryOpKind::from)
            .or(just(Token::Slash).map(BinaryOpKind::from))
            .or(just(Token::Percent).map(BinaryOpKind::from))
            .map_with_span(BinaryOp::new)
            .boxed();

        let factor = pow
            .clone()
            .foldl(
                op.clone().then(pow.clone()).repeated(),
                |lhs: Expr, (op, rhs): (BinaryOp, Expr)| {
                    Expr::new(
                        ExprKind::Binary {
                            op: op.clone(),
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        lhs.span().extend(*rhs.span()),
                    )
                },
            )
            .boxed();

        let op = just(Token::Plus)
            .map(BinaryOpKind::from)
            .or(just(Token::Minus).map(BinaryOpKind::from))
            .map_with_span(BinaryOp::new)
            .boxed();

        let term = factor
            .clone()
            .foldl(
                op.clone().then(factor.clone()).repeated(),
                |lhs: Expr, (op, rhs): (BinaryOp, Expr)| {
                    Expr::new(
                        ExprKind::Binary {
                            op: op.clone(),
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        lhs.span().extend(*rhs.span()),
                    )
                },
            )
            .boxed();

        let op = just(Token::Lt)
            .map(BinaryOpKind::from)
            .or(just(Token::Gt).map(BinaryOpKind::from))
            .or(just(Token::Leq).map(BinaryOpKind::from))
            .or(just(Token::Geq).map(BinaryOpKind::from))
            .map_with_span(BinaryOp::new)
            .boxed();

        let cmp = term
            .clone()
            .then(op.clone().then(term.clone()).or_not())
            .map(|(lhs, rhs)| match rhs {
                Some((op, rhs)) => Expr::new(
                    ExprKind::Binary {
                        op,
                        lhs: lhs.clone(),
                        rhs: rhs.clone(),
                    },
                    lhs.span().extend(*rhs.span()),
                ),
                None => lhs.clone(),
            })
            .boxed();

        let op = just(Token::Assign)
            .map(BinaryOpKind::from)
            .or(just(Token::Neq).map(BinaryOpKind::from))
            .map_with_span(BinaryOp::new)
            .boxed();

        let eq = cmp
            .clone()
            .then(op.clone().then(cmp.clone()).or_not())
            .map(|(lhs, rhs)| match rhs {
                Some((op, rhs)) => Expr::new(
                    ExprKind::Binary {
                        op,
                        lhs: lhs.clone(),
                        rhs: rhs.clone(),
                    },
                    lhs.span().extend(*rhs.span()),
                ),
                None => lhs.clone(),
            })
            .boxed();

        let and = eq
            .clone()
            .foldl(
                just(Token::And)
                    .map(BinaryOpKind::from)
                    .map_with_span(BinaryOp::new)
                    .then(eq.clone())
                    .repeated(),
                |lhs: Expr, (op, rhs): (BinaryOp, Expr)| {
                    Expr::new(
                        ExprKind::Binary {
                            op: op.clone(),
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        lhs.span().extend(*rhs.span()),
                    )
                },
            )
            .boxed();

        let or = and
            .clone()
            .foldl(
                just(Token::Or)
                    .map(BinaryOpKind::from)
                    .map_with_span(BinaryOp::new)
                    .then(and.clone())
                    .repeated(),
                |lhs: Expr, (op, rhs): (BinaryOp, Expr)| {
                    Expr::new(
                        ExprKind::Binary {
                            op: op.clone(),
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        lhs.span().extend(*rhs.span()),
                    )
                },
            )
            .boxed();

        or
    })
}

fn pattern_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Pattern, extra::Err<Rich<'a, Token, Span>>> {
    just(Token::Wildcard)
        .map_with_span(|_, span| Pattern::new(PatternKind::Wildcard, span))
        .or(ident_parser()
            .map_with_span(|ident, span| Pattern::new(PatternKind::Ident(ident), span)))
}

fn ident_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Ident, extra::Err<Rich<'a, Token, Span>>> {
    select! {
        Token::Ident(name) => name
    }
    .map_with_span(|name, span| Ident::new(name, span))
}

fn lit_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Lit, extra::Err<Rich<'a, Token, Span>>> {
    select! {
        Token::Num(n) => Lit::Num(n),
        Token::Bool(b) => Lit::Bool(b),
        Token::String(s) => Lit::String(s),
    }
}

fn curry_fn(params: Vec<Pattern>, expr: Expr) -> Expr {
    params.into_iter().rev().fold(expr, |expr, param| {
        Expr::new(
            ExprKind::Lambda {
                param: param.clone(),
                expr: expr.clone(),
            },
            param.span().extend(*expr.span()),
        )
    })
}

mod tests {
    use crate::parse;

    #[test]
    fn parse_let() {
        let src = "let x = 1";
        let (root, errors) = parse(src, false);
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_arithmetic() {
        let src = "let a = 1 + 2/3 * 3^2 - 4 / 5 % 10";
        let (root, errors) = parse(src, false);
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_boolean_cmp() {
        let src = "let a = 1 < 2 && 3 > 4 || 5 <= 6 && 7 >= 8 && !(9 == 10 && 11 != 12)";
        let (root, errors) = parse(src, false);
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_if() {
        let src = "let a = if true then 1 else 2";
        let (root, errors) = parse(src, false);
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_lambda() {
        let src = "let a = \\x -> x + 1";
        let (root, errors) = parse(src, false);
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_lambda_apply() {
        let src = "let add = (\\x y -> x + y) 1 2";
        let (root, errors) = parse(src, false);
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_let_fn() {
        let src = "let add x y = x + y";
        let (root, errors) = parse(src, false);
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_let_fn_apply() {
        let src = "let f g x = f (g x)";
        let (root, errors) = parse(src, false);
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_let_expr() {
        let src = "let x = let y = 1 in y + 1";
        let (root, errors) = parse(src, false);
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_let_fn_expr() {
        let src = "let f x = let g y = x + y in g 1";
        let (root, errors) = parse(src, false);
        insta::assert_debug_snapshot!(root);
    }
}
