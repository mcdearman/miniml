use crate::parser::Parser;

#[test]
fn parse_int() {
    let input = "523";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_float() {
    let input = "5.23";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_string() {
    let input = "\"Hello, World!\"";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_char() {
    let input = "'a'";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_bool() {
    let input = "true";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_lambda() {
    let input = "\\x -> 1";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_if() {
    let input = "if true then 1 else 2";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_let() {
    let input = "let x = 1";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_apply() {
    let input = "f x y";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_unary() {
    let input = "-f x y";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_infix() {
    let input = "1 + 2 * 3^-4";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_lss() {
    let input = "1 < 2 + x";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_gtr() {
    let input = "1 > 2 + x";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_leq() {
    let input = "1 <= 2 + x";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_geq() {
    let input = "1 >= 2 + x";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_eq() {
    let input = "1 = 2 + x";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_and_or() {
    let input = "x = y && z <= w || a > b";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_hello_world() {
    let input = "println \"Hello, World!\"";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}

#[test]
fn parse_struct_def() {
    let input = "struct Point { x, y }";
    let ast = Parser::new(input).item().expect("failed to parse");
    insta::assert_debug_snapshot!(ast);
}
