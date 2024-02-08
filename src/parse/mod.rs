use self::ast::*;
use crate::{
    lex::token::Token,
    utils::{intern::InternedString, span::Span},
};
use chumsky::{
    error::Rich,
    extra,
    input::{Input, Stream, ValueInput},
    primitive::just,
    recursive::recursive,
    select, IterParser, Parser as ChumskyParser,
};

pub mod ast;

pub fn parse<'src>(
    tokens: impl Iterator<Item = (Token, Span)> + Clone + 'src,
    repl: bool,
) -> (Option<Root>, Vec<Rich<'src, Token, Span>>) {
    let eof_span = tokens
        .clone()
        .last()
        .map(|(_, span)| span)
        .unwrap_or_default();
    let tok_stream = Stream::from_iter(tokens).spanned(eof_span);
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
                    params: vec![Pattern::new(
                        PatternKind::Ident(Ident::new(
                            InternedString::from("args"),
                            e.span().clone(),
                        )),
                        e.span().clone(),
                    )],
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
        .then_ignore(just(Token::Eq))
        .then(expr_parser())
        .map(|(pattern, expr)| DeclKind::Let { pattern, expr });

    let fn_ = just(Token::Let)
        .ignore_then(ident_parser())
        .then(pattern_parser().repeated().at_least(1).collect())
        .then_ignore(just(Token::Eq))
        .then(expr_parser())
        .map(|((name, params), expr)| DeclKind::Let {
            pattern: Pattern::new(
                PatternKind::Ident(name.clone()),
                name.span().extend(*expr.span()),
            ),
            expr: Expr::new(
                ExprKind::Lambda {
                    params,
                    expr: expr.clone(),
                },
                name.span().extend(*expr.span()),
            ),
        });

    // let datatype = just(Token::Type)
    //     .ignore_then(ident_parser())
    //     .then_ignore(just(Token::Eq))
    //     .then(datatype_kind_parser())
    //     .map(|(name, kind)| DeclKind::DataType(DataType::new(name, kind)));

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

        let list = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LBrack), just(Token::RBrack))
            .map(ExprKind::List)
            .map_with_span(Expr::new);

        let array = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::HashLBrack), just(Token::RBrack))
            .map(ExprKind::Array)
            .map_with_span(Expr::new);

        let tuple = expr
            .clone()
            .separated_by(just(Token::Comma))
            .allow_trailing()
            .collect()
            .delimited_by(just(Token::LParen), just(Token::RParen))
            .map(ExprKind::Tuple)
            .map_with_span(Expr::new);

        let let_ = just(Token::Let)
            .ignore_then(pattern_parser())
            .then_ignore(just(Token::Eq))
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
            .ignore_then(ident_parser())
            .then(pattern_parser().repeated().at_least(1).collect())
            .then_ignore(just(Token::Eq))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|(((name, params), expr), body)| ExprKind::Let {
                pattern: Pattern::new(
                    PatternKind::Ident(name.clone()),
                    name.span().extend(*body.span()),
                ),
                expr: Expr::new(
                    ExprKind::Lambda {
                        params,
                        expr: expr.clone(),
                    },
                    name.span().extend(*expr.span()),
                ),
                body,
            })
            .map_with_span(Expr::new);

        let lambda = just(Token::Backslash)
            .ignore_then(pattern_parser().repeated().at_least(1).collect())
            .then_ignore(just(Token::RArrow))
            .then(expr.clone())
            .map(|(params, expr)| ExprKind::Lambda { params, expr })
            .map_with_span(Expr::new);

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
            .or(list)
            .or(array)
            .or(tuple)
            .or(let_)
            .or(fn_)
            .or(lambda)
            .or(if_)
            .or(expr
                .clone()
                .delimited_by(just(Token::LParen), just(Token::RParen)))
            .boxed();

        let range = atom
            .clone()
            .then_ignore(just(Token::DoublePeriod))
            .then(just(Token::Eq).or_not().map(|x| x.is_some()))
            .then(atom.clone())
            .map(|((start, inclusive), end)| ExprKind::Range {
                start,
                end,
                inclusive,
                step: None,
            })
            .map_with_span(Expr::new);

        let apply = atom
            .clone()
            .then(atom.clone().repeated().collect::<Vec<_>>())
            .map(|(fun, args)| {
                if args.is_empty() {
                    fun.clone()
                } else {
                    Expr::new(
                        ExprKind::Apply {
                            fun: fun.clone(),
                            args: args.clone(),
                        },
                        fun.span().extend(*args.last().unwrap().span()),
                    )
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

        let op = just(Token::Eq)
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

fn type_hint_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, TypeHint, extra::Err<Rich<'a, Token, Span>>> {
    ident_parser()
        .map(TypeHintKind::Ident)
        .map_with_span(TypeHint::new)
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
        Token::Int(i) => Lit::Int(i),
        Token::Real(r) => Lit::Real(r),
        Token::Rational(r) => Lit::Rational(r),
        Token::Bool(b) => Lit::Bool(b),
        Token::String(s) => Lit::String(s),
        Token::Char(c) => Lit::Char(c),
    }
}

mod tests {
    use crate::{
        lex::{lexer::TokenStream, token::Token},
        parse::parse,
    };
    use itertools::Itertools;
    use logos::Logos;

    fn test_helper(src: &str) {
        let tokens = TokenStream::new(src);
        let (root, errors) = parse(tokens, true);
        if errors.len() > 0 {
            panic!("parse errors: {:?}", errors);
        }
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_let() {
        test_helper("let x = 1");
    }

    #[test]
    fn parse_arithmetic() {
        test_helper("1 + 2/3 * 3^2 - 4 / 5 % 10");
    }

    #[test]
    fn parse_boolean_cmp() {
        test_helper("1 < 2 and 3 > 4 or 5 <= 6 and 7 >= 8 and !(9 = 10 and 11 != 12)")
    }

    #[test]
    fn parse_if() {
        test_helper("if true then 1 else 2");
    }

    #[test]
    fn parse_lambda() {
        test_helper("\\x -> x + 1");
    }

    #[test]
    fn parse_lambda_apply() {
        test_helper("(\\x y -> x + y) 1 2");
    }

    #[test]
    fn parse_let_fn() {
        test_helper("let add x y = x + y");
    }

    #[test]
    fn parse_let_fn_apply() {
        test_helper("let f g x = f (g x)");
    }

    #[test]
    fn parse_let_expr() {
        test_helper("let x = let y = 1 in y + 1");
    }

    #[test]
    fn parse_let_fn_expr() {
        test_helper("let f x = let g y = x + y in g 1");
    }
}
