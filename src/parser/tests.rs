use crate::parser::{parse, token::Token, Parser};
use chumsky::{input::Stream, prelude::Input, span::SimpleSpan};
use logos::Logos;

#[test]
fn parse_int() {
    let src = "523";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

// #[test]
// fn parse_binary() {
//     let src = "0b101010";
//     let (tree, _) = parse(src);
//     insta::assert_debug_snapshot!(tree.expect("Tree is empty"));
// }

// #[test]
// fn parse_octal() {
//     let input = "0o755";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_hex() {
//     let input = "0xdeadbeef";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

#[test]
fn parse_rational() {
    let src = "5/23";
    let (tree, _) = parse(src);
    insta::assert_debug_snapshot!(tree.expect("Tree is empty"));
}

#[test]
fn parse_float() {
    let src = "5.23";
    let (tree, _) = parse(src);
    insta::assert_debug_snapshot!(tree.expect("Tree is empty"));
}

#[test]
fn parse_string() {
    let src = "\"Hello, World!\"";
    let (tree, _) = parse(src);
    insta::assert_debug_snapshot!(tree.expect("Tree is empty"));
}

#[test]
fn parse_char() {
    let src = "'a'";
    let (tree, _) = parse(src);
    insta::assert_debug_snapshot!(tree.expect("Tree is empty"));
}

#[test]
fn parse_bool() {
    let src = "true";
    let (tree, _) = parse(src);
    insta::assert_debug_snapshot!(tree.expect("Tree is empty"));
}

#[test]
fn parse_list() {
    let src = "[1, 2, 3]";
    let (tree, _) = parse(src);
    insta::assert_debug_snapshot!(tree.expect("Tree is empty"));
}

#[test]
fn parse_tuple() {
    let src = "(1, 2, 3)";
    let (tree, _) = parse(src);
    insta::assert_debug_snapshot!(tree.expect("Tree is empty"));
}

// #[test]
// fn parse_lambda() {
//     let input = "\\x -> 1";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_if() {
//     let input = "if true then 1 else 2";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_let_decl() {
//     let input = "let x = 1";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_let_expr() {
//     let input = "let x = 1 in x + 1";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_fn_decl() {
//     let input = "fn f x y = x + y";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_fn_expr() {
//     let input = "fn f x y = x + y in f 1 2";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_apply() {
//     let input = "f x y";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_unary() {
//     let input = "-f x y";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_infix() {
//     let input = "1 + 2 * 3^-4";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_lss() {
//     let input = "1 < 2 + x";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_gtr() {
//     let input = "1 > 2 + x";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_leq() {
//     let input = "1 <= 2 + x";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_geq() {
//     let input = "1 >= 2 + x";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_eq() {
//     let input = "1 = 2 + x";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_and_or() {
//     let input = "x = y && z <= w || a > b";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_hello_world() {
//     let input = "println \"Hello, World!\"";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_gcd() {
//     let input = "fn gcd a b = if b = 0 then a else gcd b (a % b) in gcd 18 24";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// // #[test]
// // fn parse_map() {
// //     let input = "{ x: 1, y: 2, z: 3 }";
// //     let ast = Parser::new(input).item().expect("failed to parse");
// //     insta::assert_debug_snapshot!(ast);
// // }

// #[test]
// fn parse_data() {
//     let input = "data Point = { x, y }";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }
