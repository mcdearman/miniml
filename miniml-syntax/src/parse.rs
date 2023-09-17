use crate::{ast::Expr, token::Token};
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
use miniml_util::intern::InternedString;

pub fn parser<'a, I: ValueInput<'a, Token = Token, Span = SimpleSpan>>(
) -> impl Parser<'a, I, Expr, extra::Err<Rich<'a, Token>>> {
    let ident = select! { Token::Ident(name) => name };

    let expr = recursive(|expr| {
        let inline_expr = recursive(|inline_expr| {
            let val = select! {
                Token::Int(n) => Expr::Int(n),
            }
            .boxed();

            // parse let
            let let_ = just(Token::Let)
                .ignore_then(ident)
                .then_ignore(just(Token::Eq))
                .then(inline_expr.clone())
                .then_ignore(just(Token::In))
                .then(expr.clone())
                .map(|((name, val), body)| {
                    Expr::Let(InternedString::from(name), Box::new(val), Box::new(body))
                })
                .boxed();

            // parse curry lambda
            let lambda = just(Token::Backslash)
                .ignore_then(
                    ident
                        .repeated()
                        .foldr(just(Token::Arrow).ignore_then(expr.clone()), |arg, body| {
                            Expr::Lambda(InternedString::from(arg), Box::new(body))
                        }),
                )
                .boxed();

            let atom = choice((
                val,
                let_,
                lambda,
                ident.map(Expr::Var),
                expr.clone()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
            ))
            .boxed();

            // parse function application
            let apply = atom
                .clone()
                .foldl(atom.clone().repeated(), |f, arg| {
                    Expr::Apply(Box::new(f), Box::new(arg))
                })
                .boxed();

            // parse arithmetic
            let op = just(Token::Mul).or(just(Token::Div)).boxed();

            let product = apply
                .clone()
                .foldl(
                    op.clone().then(apply.clone()).repeated(),
                    |l, (op, r)| match op {
                        Token::Mul => Expr::Mul(Box::new(l), Box::new(r)),
                        Token::Div => Expr::Div(Box::new(l), Box::new(r)),
                        _ => unreachable!(),
                    },
                )
                .boxed();

            let op = just(Token::Add).or(just(Token::Sub)).boxed();
            let sum = product
                .clone()
                .foldl(
                    op.clone().then(product.clone()).repeated(),
                    |l, (op, r)| match op {
                        Token::Add => Expr::Add(Box::new(l), Box::new(r)),
                        Token::Sub => Expr::Sub(Box::new(l), Box::new(r)),
                        _ => unreachable!(),
                    },
                )
                .boxed();

            sum
        });

        inline_expr
    });

    expr
}

mod tests {
    // #[test]
    // fn test_int() {
    //     let mut parser = Parser::new("123");
    //     let expr = parser.expr().expect("parse error");
    //     insta::assert_debug_snapshot!(expr);
    // }

    // #[test]
    // fn test_binary() {
    //     let mut parser = Parser::new("0b10101");
    //     let expr = parser.expr().expect("parse error");
    //     insta::assert_debug_snapshot!(expr);
    // }

    // #[test]
    // fn test_rational() {
    //     let mut parser = Parser::new("123/456");
    //     let expr = parser.expr().expect("parse error");
    //     insta::assert_debug_snapshot!(expr);
    // }

    // #[test]
    // fn test_real() {
    //     let mut parser = Parser::new("123.456");
    //     let expr = parser.expr().expect("parse error");
    //     insta::assert_debug_snapshot!(expr);
    // }

    // #[test]
    // fn test_complex() {
    //     let mut parser = Parser::new("123.456i");
    //     let expr = parser.expr().expect("parse error");
    //     insta::assert_debug_snapshot!(expr);
    // }

    // #[test]
    // fn test_unit() {
    //     let mut parser = Parser::new("()");
    //     let expr = parser.expr().expect("parse error");
    //     insta::assert_debug_snapshot!(expr);
    // }

    // #[test]
    // fn test_neg() {
    //     let mut parser = Parser::new("-x");
    //     let expr = parser.expr().expect("parse error");
    //     insta::assert_debug_snapshot!(expr);
    // }

    // #[test]
    // fn test_add() {
    //     let mut parser = Parser::new("1 + 2");
    //     let expr = parser.expr().expect("parse error");
    //     insta::assert_debug_snapshot!(expr);
    // }

    // #[test]
    // fn test_infix() {
    //     let mut parser = Parser::new("1 + 2 * 3 / 4^1/2 - 2.5");
    //     let expr = parser.expr().expect("parse error");
    //     insta::assert_debug_snapshot!(expr);
    // }

    // #[test]
    // fn test_if() {
    //     let mut parser = Parser::new("if x then y else z");
    //     let expr = parser.expr().expect("parse error");
    //     insta::assert_debug_snapshot!(expr);
    // }

    // #[test]
    // fn test_elif() {
    //     let mut parser = Parser::new("if x then y elif a then b else z");
    //     let expr = parser.expr().expect("parse error");
    //     insta::assert_debug_snapshot!(expr);
    // }

    // #[test]
    // fn test_if_atom() {
    //     let mut parser = Parser::new("add 1 if x then y else z");
    //     let expr = parser.expr().expect("parse error");
    //     insta::assert_debug_snapshot!(expr);
    // }

    // #[test]
    // fn test_lambda() {
    //     let mut parser = Parser::new("\\x y -> x + y");
    //     let expr = parser.expr().expect("parse error");
    //     insta::assert_debug_snapshot!(expr);
    // }

    // #[test]
    // fn test_let_expr() {
    //     let mut parser = Parser::new("let x = 1 in x + 1");
    //     let expr = parser.expr().expect("parse error");
    //     insta::assert_debug_snapshot!(expr);
    // }

    // // #[test]
    // // fn test_const() {
    // //     let mut parser = Parser::new("const x = 1");
    // //     let (root, errors) = parser.parse();
    // //     if !errors.is_empty() {
    // //         panic!("parse error: {:?}", errors);
    // //     }
    // //     insta::assert_debug_snapshot!(root);
    // // }

    // #[test]
    // fn test_let() {
    //     let mut parser = Parser::new("let x = 1");
    //     let (root, errors) = parser.parse();
    //     if !errors.is_empty() {
    //         panic!("parse error: {:?}", errors);
    //     }
    //     insta::assert_debug_snapshot!(root);
    // }

    // #[test]
    // fn test_fn_add() {
    //     let mut parser = Parser::new("let add x y = x + y");
    //     let (root, errors) = parser.parse();
    //     if !errors.is_empty() {
    //         panic!("parse error: {:?}", errors);
    //     }
    //     insta::assert_debug_snapshot!(root);
    // }
}
