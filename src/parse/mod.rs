use self::ast::*;
use crate::{
    lex::token::Token,
    utils::{ident::Ident, intern::InternedString, span::Span},
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
        .map_with(|decls, e| Root {
            decls,
            span: e.span(),
        })
}

fn repl_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Root, extra::Err<Rich<'a, Token, Span>>> {
    expr_parser()
        .map(|e| {
            DeclKind::Fn(
                Ident::new(InternedString::from("main"), e.span),
                vec![
                    Ident::new(InternedString::from("args"), e.span),
                    // TypeHint::new(TypeHintKind::Unit, e.span().clone()),
                ],
                // TypeHint::new(TypeHintKind::Unit, e.span().clone()),
                e.clone(),
            )
        })
        .map_with(|kind, e| Decl {
            kind,
            span: e.span(),
        })
        .or(decl_parser())
        .map_with(|decl, e| Root {
            decls: vec![decl],
            span: e.span(),
        })
        .boxed()
}

fn decl_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Decl, extra::Err<Rich<'a, Token, Span>>> {
    // let record = just(Token::Type)
    //     .ignore_then(ident_parser())
    //     .then_ignore(just(Token::Eq))
    //     .then(
    //         ident_parser()
    //             .then_ignore(just(Token::Colon))
    //             .then(type_hint_parser())
    //             .separated_by(just(Token::Comma))
    //             .allow_trailing()
    //             .at_least(1)
    //             .collect()
    //             .delimited_by(just(Token::LBrace), just(Token::RBrace)),
    //     )
    //     .map_with(|(name, fields), e| {
    //         DeclKind::DataType(DataType::new(
    //             name,
    //             DataTypeKind::Record { fields },
    //             e.span(),
    //         ))
    //     });

    let let_ = just(Token::Let)
        .ignore_then(ident_parser())
        .then_ignore(just(Token::Eq))
        .then(expr_parser())
        .map(|(name, expr)| DeclKind::Let(name, expr));

    let fn_ = just(Token::Let)
        .ignore_then(ident_parser())
        .then(ident_parser().repeated().at_least(1).collect())
        .then_ignore(just(Token::Eq))
        .then(expr_parser())
        .map(|((name, params), expr)| DeclKind::Fn(name, params, expr));

    let_.or(fn_)
        // .or(record)
        .map_with(|kind, e| Decl {
            kind,
            span: e.span(),
        })
}

fn expr_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Expr, extra::Err<Rich<'a, Token, Span>>> {
    recursive(|expr| {
        let unit = just(Token::LParen)
            .ignore_then(just(Token::RParen))
            .map(|_| ExprKind::Unit);

        let lit = lit_parser().map(|lit| ExprKind::Lit(lit));

        let ident = ident_parser().map(|ident| ExprKind::Var(ident));

        let simple = unit
            .or(lit)
            .or(ident)
            .map_with(|kind, e| Expr::new(kind, e.span()))
            .or(just(Token::LParen)
                .ignore_then(expr.clone())
                .then_ignore(just(Token::RParen)))
            .boxed();

        let lambda = just(Token::Backslash)
            .ignore_then(ident_parser().repeated().at_least(1).collect())
            .then_ignore(just(Token::RArrow))
            .then(expr.clone())
            .map(|(params, expr)| ExprKind::Lambda(params, expr));

        let if_ = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .then_ignore(just(Token::Else))
            .then(expr.clone())
            .map(|((cond, then), else_)| ExprKind::If(cond, then, else_));

        let let_ = just(Token::Let)
            .ignore_then(ident_parser())
            .then_ignore(just(Token::Eq))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((name, expr), body)| ExprKind::Let(name, expr, body));

        let fn_ = just(Token::Let)
            .ignore_then(ident_parser())
            .then(ident_parser().repeated().at_least(1).collect())
            .then_ignore(just(Token::Eq))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|(((name, params), expr), body)| ExprKind::Fn(name, params, expr, body));

        let list = just(Token::LBrack)
            .ignore_then(
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect(),
            )
            .then_ignore(just(Token::RBrack))
            .map(|exprs| ExprKind::List(exprs));

        let atom = let_
            .or(fn_)
            .or(lambda)
            .or(if_)
            .or(list)
            .map_with(|kind, e| Expr::new(kind, e.span()))
            .or(simple)
            .boxed();

        let apply = atom
            .clone()
            .then(atom.clone().repeated().collect::<Vec<_>>())
            .map(|(fun, args)| {
                if args.is_empty() {
                    *fun.kind
                } else {
                    ExprKind::Apply(fun, args)
                }
            })
            .map_with(|kind, e| Expr::new(kind, e.span()));

        let op = just(Token::Minus)
            .or(just(Token::Not))
            .map(UnaryOpKind::from)
            .map_with(|kind, e| UnaryOp {
                kind,
                span: e.span(),
            });

        let unary = op.clone().repeated().foldr(apply, |op, expr| {
            Expr::new(
                ExprKind::UnaryOp(op, expr.clone()),
                op.span.extend(expr.span),
            )
        });

        let op = just(Token::Caret)
            .map(BinaryOpKind::from)
            .map_with(|kind, e| BinaryOp {
                kind,
                span: e.span(),
            });

        let pow = unary
            .clone()
            .foldl(
                op.clone().then(unary.clone()).repeated(),
                |lhs: Expr, (op, rhs): (BinaryOp, Expr)| {
                    Expr::new(
                        ExprKind::BinaryOp(op, lhs.clone(), rhs.clone()),
                        lhs.span.extend(rhs.span),
                    )
                },
            )
            .boxed();

        let op = just(Token::Star)
            .or(just(Token::Slash))
            .or(just(Token::Percent))
            .map(BinaryOpKind::from)
            .map_with(|kind, e| BinaryOp {
                kind,
                span: e.span(),
            });

        let factor = pow
            .clone()
            .foldl(
                op.clone().then(pow.clone()).repeated(),
                |lhs: Expr, (op, rhs): (BinaryOp, Expr)| {
                    Expr::new(
                        ExprKind::BinaryOp(op, lhs.clone(), rhs.clone()),
                        lhs.span.extend(rhs.span),
                    )
                },
            )
            .boxed();

        let op = just(Token::Plus)
            .or(just(Token::Minus))
            .map(BinaryOpKind::from)
            .map_with(|kind, e| BinaryOp {
                kind,
                span: e.span(),
            })
            .boxed();

        let term = factor
            .clone()
            .foldl(
                op.clone().then(factor.clone()).repeated(),
                |lhs: Expr, (op, rhs): (BinaryOp, Expr)| {
                    Expr::new(
                        ExprKind::BinaryOp(op, lhs.clone(), rhs.clone()),
                        lhs.span.extend(rhs.span),
                    )
                },
            )
            .boxed();

        let op = just(Token::Lt)
            .or(just(Token::Gt))
            .or(just(Token::Leq))
            .or(just(Token::Geq))
            .map(BinaryOpKind::from)
            .map_with(|kind, e| BinaryOp {
                kind,
                span: e.span(),
            })
            .boxed();

        let cmp = term
            .clone()
            .then(op.clone().then(term.clone()).or_not())
            .map(|(lhs, rhs)| match rhs {
                Some((op, rhs)) => Expr::new(
                    ExprKind::BinaryOp(op, lhs.clone(), rhs.clone()),
                    lhs.span.extend(rhs.span),
                ),
                None => lhs.clone(),
            })
            .boxed();

        let op = just(Token::Eq)
            .or(just(Token::Neq))
            .map(BinaryOpKind::from)
            .map_with(|kind, e| BinaryOp {
                kind,
                span: e.span(),
            })
            .boxed();

        let eq = cmp
            .clone()
            .then(op.clone().then(cmp.clone()).or_not())
            .map(|(lhs, rhs)| match rhs {
                Some((op, rhs)) => Expr::new(
                    ExprKind::BinaryOp(op, lhs.clone(), rhs.clone()),
                    lhs.span.extend(rhs.span),
                ),
                None => lhs.clone(),
            })
            .boxed();

        let and = eq
            .clone()
            .foldl(
                just(Token::And).ignore_then(eq.clone()).repeated(),
                |lhs, rhs| {
                    Expr::new(
                        ExprKind::And(lhs.clone(), rhs.clone()),
                        lhs.span.extend(rhs.span),
                    )
                },
            )
            .boxed();

        let or = and
            .clone()
            .foldl(
                just(Token::Or).ignore_then(and.clone()).repeated(),
                |lhs, rhs| {
                    Expr::new(
                        ExprKind::Or(lhs.clone(), rhs.clone()),
                        lhs.span.extend(rhs.span),
                    )
                },
            )
            .boxed();

        or
    })
}

// fn type_hint_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
// ) -> impl ChumskyParser<'a, I, TypeHint, extra::Err<Rich<'a, Token, Span>>> {
//     let simple = just(Token::Colon)
//         .ignore_then(ident_parser())
//         .map(|ident| match ident.as_str() {
//             "Int" => TypeHintKind::Int,
//             "Bool" => TypeHintKind::Bool,
//             _ => TypeHintKind::Unit,
//         })
//         .map_with(|ty, e| TypeHint::new(ty, e.span()));

//     todo!()
// }

fn ident_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Ident, extra::Err<Rich<'a, Token, Span>>> {
    select! {
        Token::Ident(name) => name
    }
    .map_with(|name, e| Ident::new(name, e.span()))
}

fn lit_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Lit, extra::Err<Rich<'a, Token, Span>>> {
    select! {
        Token::Int(i) => Lit::Int(i),
        Token::Rational(r) => Lit::Rational(r),
        Token::Real(r) => Lit::Real(r),
        Token::Bool(b) => Lit::Bool(b),
        Token::String(s) => Lit::String(s),
        Token::Char(c) => Lit::Char(c),
    }
}

// mod tests {
//     use crate::{
//         lex::{token::Token, token_stream::TokenStream},
//         parse::parse,
//     };
//     use itertools::Itertools;
//     use logos::Logos;

//     fn test_helper(src: &str) {
//         let tokens = TokenStream::new(src);
//         let (root, errors) = parse(tokens, true);
//         if errors.len() > 0 {
//             panic!("parse errors: {:?}", errors);
//         }
//         insta::assert_debug_snapshot!(root);
//     }

//     #[test]
//     fn parse_let() {
//         test_helper("let x = 1");
//     }

//     #[test]
//     fn parse_arithmetic() {
//         test_helper("1 + 2/3 * 3^2 - 4 / 5 % 10");
//     }

//     #[test]
//     fn parse_boolean_cmp() {
//         test_helper("1 < 2 and 3 > 4 or 5 <= 6 and 7 >= 8 and !(9 = 10 and 11 != 12)")
//     }

//     #[test]
//     fn parse_if() {
//         test_helper("if true then 1 else 2");
//     }

//     #[test]
//     fn parse_lambda() {
//         test_helper("\\x -> x + 1");
//     }

//     #[test]
//     fn parse_lambda_apply() {
//         test_helper("(\\x y -> x + y) 1 2");
//     }

//     #[test]
//     fn parse_let_fn() {
//         test_helper("let add x y = x + y");
//     }

//     #[test]
//     fn parse_let_fn_apply() {
//         test_helper("let f g x = f (g x)");
//     }

//     #[test]
//     fn parse_let_expr() {
//         test_helper("let x = let y = 1 in y + 1");
//     }

//     #[test]
//     fn parse_let_fn_expr() {
//         test_helper("let f x = let g y = x + y in g 1");
//     }
// }
