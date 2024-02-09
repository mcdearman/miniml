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

    let record_def = just(Token::Type)
        .ignore_then(ident_parser())
        .then_ignore(just(Token::Eq))
        .then(record_parser())
        .map(|(name, fields)| {
            DeclKind::DataType(DataType::new(
                name.clone(),
                DataTypeKind::Record {
                    fields: fields.clone(),
                },
                name.span().extend(*fields.last().unwrap().1.span()),
            ))
        });

    // sumType = "type" ident "=" sumTypeCase ("|" sumTypeCase)*
    let sum_type_def = just(Token::Type)
        .ignore_then(ident_parser())
        .then_ignore(just(Token::Eq))
        .then(sum_type_case_parser())
        .then(
            just(Token::Bar)
                .ignore_then(sum_type_case_parser())
                .repeated()
                .collect::<Vec<_>>(),
        )
        .map(|((name, first), cases)| {
            DeclKind::DataType(DataType::new(
                name.clone(),
                DataTypeKind::Sum {
                    cases: std::iter::once(first.clone())
                        .chain(cases.clone())
                        .collect(),
                },
                name.span().extend(match &cases.last().unwrap_or(&first).1 {
                    Some(hint) => hint.span().clone(),
                    None => first.0.span().clone(),
                }),
            ))
        });

    // productType = "type" ident "=" ident typeHint+
    let product_type_def = just(Token::Type)
        .ignore_then(ident_parser())
        .then_ignore(just(Token::Eq))
        .then(ident_parser())
        .then(
            type_hint_parser()
                .repeated()
                .at_least(1)
                .collect::<Vec<_>>(),
        )
        .map(|((name, constructor), hints)| {
            DeclKind::DataType(DataType::new(
                name.clone(),
                DataTypeKind::Product {
                    constructor,
                    fields: hints.clone(),
                },
                name.span().extend(*hints.last().unwrap().span()),
            ))
        });

    let_.or(fn_)
        .or(record_def)
        .or(product_type_def)
        .or(sum_type_def)
        .map_with_span(Decl::new)
        .boxed()
}

fn record_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Vec<(Ident, TypeHint)>, extra::Err<Rich<'a, Token, Span>>> {
    just(Token::LBrace)
        .ignore_then(
            ident_parser()
                .then_ignore(just(Token::Colon))
                .then(type_hint_parser())
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect(),
        )
        .then_ignore(just(Token::RBrace))
}

fn sum_type_case_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, (Ident, Option<SumTypeCaseHint>), extra::Err<Rich<'a, Token, Span>>>
{
    ident_parser()
        .then(sum_type_case_hint_parser().or_not())
        .map(|(ident, hint)| (ident, hint))
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
            .at_least(2)
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

        let match_ = just(Token::Match)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::With))
            .then(
                just(Token::Bar)
                    .ignore_then(pattern_parser())
                    .then_ignore(just(Token::RArrow))
                    .then(expr.clone())
                    .repeated()
                    .collect(),
            )
            .map(|(expr, cases)| ExprKind::Match { expr, cases })
            .map_with_span(Expr::new);

        let record_field = ident_parser()
            .then_ignore(just(Token::Eq))
            .then(expr.clone());

        let record = just(Token::LBrace)
            .ignore_then(
                record_field
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect(),
            )
            .then_ignore(just(Token::RBrace))
            .map(|fields| ExprKind::Record { fields })
            .map_with_span(Expr::new);

        let atom = ident_parser()
            .map(ExprKind::Ident)
            .map_with_span(Expr::new)
            .or(unit)
            .or(lit)
            .or(list)
            .or(array)
            .or(tuple)
            .or(record)
            .or(let_)
            .or(fn_)
            .or(lambda)
            .or(if_)
            .or(match_)
            .or(expr
                .clone()
                .delimited_by(just(Token::LParen), just(Token::RParen)))
            .boxed();

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

        let op = just(Token::Period)
            .map(BinaryOpKind::from)
            .map_with_span(BinaryOp::new);

        // dot = apply "." ident
        let dot = apply
            .clone()
            .foldl(
                op.clone().then(ident_parser()).repeated(),
                |lhs, (op, rhs)| {
                    Expr::new(
                        ExprKind::Binary {
                            op,
                            lhs: lhs.clone(),
                            rhs: Expr::new(ExprKind::Ident(rhs.clone()), rhs.span().clone()),
                        },
                        lhs.span().extend(rhs.span().clone()),
                    )
                },
            )
            .boxed();

        let op = just(Token::Minus)
            .map(UnaryOpKind::from)
            .or(just(Token::Bang).map(UnaryOpKind::from))
            .map_with_span(UnaryOp::new)
            .boxed();

        let unary = op
            .clone()
            .repeated()
            .foldr(dot.clone(), |op, expr| {
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

        let op = just(Token::DoubleColon)
            .map(BinaryOpKind::from)
            .map_with_span(BinaryOp::new)
            .boxed();

        let pair = or
            .clone()
            .then(op.clone().then(or.clone()).or_not())
            .map(|(head, rhs)| match rhs {
                Some((op, tail)) => Expr::new(
                    ExprKind::Binary {
                        op,
                        lhs: head.clone(),
                        rhs: tail.clone(),
                    },
                    head.span().extend(*tail.span()),
                ),
                None => head.clone(),
            })
            .boxed();

        pair
    })
}

fn pattern_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Pattern, extra::Err<Rich<'a, Token, Span>>> {
    recursive(|pat| {
        just(Token::Wildcard)
            .map(|_| PatternKind::Wildcard)
            .or(lit_parser().map(PatternKind::Lit))
            .or(ident_parser().map(PatternKind::Ident))
            .or(just(Token::LParen)
                .ignore_then(
                    pat.clone()
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect(),
                )
                .then_ignore(just(Token::RParen))
                .map(PatternKind::Tuple))
            .or(just(Token::LBrack)
                .ignore_then(
                    pat.clone()
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect(),
                )
                .then_ignore(just(Token::RBrack))
                .map(PatternKind::List))
            .map_with_span(Pattern::new)
            .then(just(Token::DoubleColon).ignore_then(pat.clone()).or_not())
            .map(|(head, tail)| match tail {
                Some(tail) => PatternKind::Pair(head, tail.clone()),
                None => head.kind().clone(),
            })
            .map_with_span(Pattern::new)
            .boxed()
    })
}

fn sum_type_case_hint_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, SumTypeCaseHint, extra::Err<Rich<'a, Token, Span>>> {
    type_hint_parser()
        .repeated()
        .at_least(2)
        .collect()
        .map(SumTypeCaseHintKind::Product)
        .or(type_hint_parser().map(SumTypeCaseHintKind::TypeHint))
        .or(just(Token::LBrace)
            .ignore_then(
                ident_parser()
                    .then_ignore(just(Token::Colon))
                    .then(type_hint_parser())
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect(),
            )
            .then_ignore(just(Token::RBrace))
            .map(SumTypeCaseHintKind::Record))
        .map_with_span(SumTypeCaseHint::new)
}

fn type_hint_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, TypeHint, extra::Err<Rich<'a, Token, Span>>> {
    recursive(|hint| {
        ident_parser()
            .map(|ident| match ident.name().as_ref() {
                "Int" => TypeHintKind::Int,
                "Byte" => TypeHintKind::Byte,
                "Real" => TypeHintKind::Real,
                "Rational" => TypeHintKind::Rational,
                "Bool" => TypeHintKind::Bool,
                "String" => TypeHintKind::String,
                "Char" => TypeHintKind::Char,
                "Unit" => TypeHintKind::Unit,
                _ => TypeHintKind::Ident(ident),
            })
            .or(just(Token::LBrack)
                .ignore_then(hint.clone())
                .then_ignore(just(Token::RBrack))
                .map(TypeHintKind::List))
            .or(just(Token::HashLBrack)
                .ignore_then(hint.clone())
                .then_ignore(just(Token::RBrack))
                .map(TypeHintKind::Array))
            .or(just(Token::LParen)
                .ignore_then(
                    hint.clone()
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect(),
                )
                .then_ignore(just(Token::RParen))
                .map(TypeHintKind::Tuple))
            .map_with_span(TypeHint::new)
            .then(
                hint.clone()
                    .repeated()
                    .collect::<Vec<_>>()
                    .then_ignore(just(Token::RArrow))
                    .then(hint.clone())
                    .or_not(),
            )
            .map(|(first, rest)| match rest {
                Some((params, ret)) => TypeHintKind::Fn(
                    std::iter::once(first.clone())
                        .chain(params.clone())
                        .collect(),
                    ret.clone(),
                ),
                None => first.kind().clone(),
            })
            .map_with_span(TypeHint::new)
            .boxed()
    })
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
        lex::{token::Token, token_stream::TokenStream},
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
