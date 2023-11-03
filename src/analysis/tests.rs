use std::default;

use chumsky::{input::Stream, prelude::Input, span::SimpleSpan, Parser};
use logos::Logos;

use crate::{
    intern::InternedString,
    parser::Expr,
    typing::{default_ctx, type_inference},
};

// ============================================================================
//                                  Parser
// ============================================================================

// #[test]
// fn test_parse_let() {
//     let src = "let x = 5 in x";
//     let lex = Token::lexer(&src)
//         .spanned()
//         .map(|(tok, span)| (tok, SimpleSpan::from(span)));
//     let tok_stream = Stream::from_iter(lex).spanned(SimpleSpan::from(src.len()..src.len()));
//     let ast = parser()
//         .parse(tok_stream)
//         .into_result()
//         .expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn test_parse_multiline_let() {
//     let src = "let x =\n5 in x";
//     let lex = Token::lexer(&src)
//         .spanned()
//         .map(|(tok, span)| (tok, SimpleSpan::from(span)));
//     let tok_stream = Stream::from_iter(lex).spanned(SimpleSpan::from(src.len()..src.len()));
//     let ast = parser()
//         .parse(tok_stream)
//         .into_result()
//         .expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn test_parse_apply() {
//     let src = "let f = \\x -> x in f 5";
//     let lex = Token::lexer(&src)
//         .spanned()
//         .map(|(tok, span)| (tok, SimpleSpan::from(span)));
//     let tok_stream = Stream::from_iter(lex).spanned(SimpleSpan::from(src.len()..src.len()));
//     let ast = parser()
//         .parse(tok_stream)
//         .into_result()
//         .expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn test_parse_multiline_apply() {
//     let src = "let f =\n\\x -> x in f 5";
//     let lex = Token::lexer(&src)
//         .spanned()
//         .map(|(tok, span)| (tok, SimpleSpan::from(span)));
//     let tok_stream = Stream::from_iter(lex).spanned(SimpleSpan::from(src.len()..src.len()));
//     let ast = parser()
//         .parse(tok_stream)
//         .into_result()
//         .expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// ============================================================================
//                               Type Inference
// ============================================================================

#[test]
fn test_infer_int() {
    let ast = Expr::Num(5.into());
    let ty = type_inference(default_ctx(), ast).expect("failed to infer");
    insta::assert_debug_snapshot!(ty);
}

#[test]
fn test_infer_bool() {
    let ast = Expr::Bool(true);
    let ty = type_inference(default_ctx(), ast).expect("failed to infer");
    insta::assert_debug_snapshot!(ty);
}

#[test]
fn test_infer_add() {
    let ast = Expr::Add(Box::new(Expr::Num(5.into())), Box::new(Expr::Num(3.into())));
    let ty = type_inference(default_ctx(), ast).expect("failed to infer");
    insta::assert_debug_snapshot!(ty);
}

#[test]
fn test_infer_lambda_id() {
    let ast = Expr::Lambda(
        InternedString::from("x"),
        Box::new(Expr::Ident(InternedString::from("x"))),
    );
    let ty = type_inference(default_ctx(), ast).expect("failed to infer");
    insta::assert_debug_snapshot!(ty);
}

#[test]
fn test_infer_lambda_add() {
    // \x -> x + 1
    let ast = Expr::Lambda(
        InternedString::from("x"),
        Box::new(Expr::Add(
            Box::new(Expr::Ident(InternedString::from("x"))),
            Box::new(Expr::Num(1.into())),
        )),
    );
    let ty = type_inference(default_ctx(), ast).expect("failed to infer");
    insta::assert_debug_snapshot!(ty);
}

#[test]
fn test_infer_lambda_add_vars() {
    // \x y -> x + y
    let ast = Expr::Lambda(
        InternedString::from("x"),
        Box::new(Expr::Lambda(
            InternedString::from("y"),
            Box::new(Expr::Add(
                Box::new(Expr::Ident(InternedString::from("x"))),
                Box::new(Expr::Ident(InternedString::from("y"))),
            )),
        )),
    );
    let ty = type_inference(default_ctx(), ast).expect("failed to infer");
    insta::assert_debug_snapshot!(ty);
}

#[test]
fn test_infer_apply_lambda_add() {
    // (\x -> x + 1) 5
    let ast = Expr::Apply(
        Box::new(Expr::Lambda(
            InternedString::from("x"),
            Box::new(Expr::Add(
                Box::new(Expr::Ident(InternedString::from("x"))),
                Box::new(Expr::Num(1.into())),
            )),
        )),
        Box::new(Expr::Num(5.into())),
    );
    let ty = type_inference(default_ctx(), ast).expect("failed to infer");
    insta::assert_debug_snapshot!(ty);
}

#[test]
fn test_infer_lambda_apply() {
    // \f x -> f x
    // ('a -> 'b) -> 'a -> 'b
    let ast = Expr::Lambda(
        InternedString::from("f"),
        Box::new(Expr::Lambda(
            InternedString::from("x"),
            Box::new(Expr::Apply(
                Box::new(Expr::Ident(InternedString::from("f"))),
                Box::new(Expr::Ident(InternedString::from("x"))),
            )),
        )),
    );
    let ty = type_inference(default_ctx(), ast).expect("failed to infer");
    insta::assert_debug_snapshot!(ty);
}

#[test]
fn test_infer_partial_apply_lambda_apply() {
    // (\f x -> f x) (\x -> x + 1)
    // Int -> Int
    let ast = Expr::Apply(
        Box::new(Expr::Lambda(
            InternedString::from("f"),
            Box::new(Expr::Lambda(
                InternedString::from("x"),
                Box::new(Expr::Apply(
                    Box::new(Expr::Ident(InternedString::from("f"))),
                    Box::new(Expr::Ident(InternedString::from("x"))),
                )),
            )),
        )),
        Box::new(Expr::Lambda(
            InternedString::from("x"),
            Box::new(Expr::Add(
                Box::new(Expr::Ident(InternedString::from("x"))),
                Box::new(Expr::Num(1.into())),
            )),
        )),
    );
    let ty = type_inference(default_ctx(), ast).expect("failed to infer");
    insta::assert_debug_snapshot!(ty);
}

#[test]
fn test_infer_ty_var() {
    // \a b c d e f g h i j k l m n o p q r s t u v w x y z a1 -> a
    let ty = type_inference(default_ctx(), ast).expect("failed to infer");
    insta::assert_debug_snapshot!(ty);
}
