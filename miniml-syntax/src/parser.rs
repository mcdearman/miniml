use crate::{
    ast::{Expr, Item, PrefixOp, Root},
    error::SyntaxError,
    lex::{Token, TokenStream},
};
use miniml_util::span::{Span, Spanned};

#[derive(Debug)]
pub struct Parser {
    tokens: TokenStream,
}

impl Parser {
    pub fn new(tokens: TokenStream) -> Self {
        Self { tokens }
    }

    pub fn parse(&mut self) -> Result<Spanned<Root>, Vec<Spanned<SyntaxError>>> {
        self.root()
    }

    fn root(&mut self) -> Result<Spanned<Root>, Vec<Spanned<SyntaxError>>> {
        let mut errors = Vec::new();
        let mut items = Vec::new();
        let start = self.tokens.peek().1.start;

        while !self.tokens.at(&Token::Eof) {
            match self.item() {
                Ok(item) => items.push(item),
                Err(err) => errors.push(err),
            }
        }

        let end = self.tokens.peek().1.end;
        if errors.is_empty() {
            Ok((Root { items }, Span::new(start, end)))
        } else {
            Err(errors)
        }
    }

    fn item(&mut self) -> Result<Spanned<Item>, Spanned<SyntaxError>> {
        let start = self.tokens.peek().1.start;
        match self.tokens.peek().0 {
            _ => Ok((
                Item::Expr(self.expr()?),
                Span::new(start, self.tokens.peek().1.end),
            )),
        }
    }

    fn expr(&mut self) -> Result<Spanned<Expr>, Spanned<SyntaxError>> {
        let start = self.tokens.peek().1.start;
        match self.tokens.peek().0 {
            Token::Ident(name) => {
                self.tokens.next();
                Ok((
                    Expr::Ident(name),
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            Token::Int(i) => Ok((Expr::Int(i), Span::new(start, self.tokens.next().1.end))),
            Token::Real(r) => Ok((Expr::Real(r), Span::new(start, self.tokens.next().1.end))),
            Token::String(s) => {
                self.tokens.next();
                Ok((Expr::String(s), Span::new(start, self.tokens.peek().1.end)))
            }
            Token::Sub => {
                self.tokens.next();
                Ok((
                    Expr::Prefix {
                        op: PrefixOp::Neg,
                        expr: Box::new(self.expr()?),
                    },
                    Span::new(start, self.tokens.peek().1.end),
                ))
            }
            _ => todo!(),
        }
    }
}

// #[test]
// fn parse_int() {
//     let src = "523";
//     let (tree, _) = parse(src);
//     insta::assert_debug_snapshot!(tree.expect("Tree is empty"));
// }

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

// fn parse_rational() {
//     let src = "5/23";
//     let (tree, _) = parse(src);
//     insta::assert_debug_snapshot!(tree.expect("Tree is empty"));
// }

// #[test]
// fn parse_float() {
//     let src = "5.23";
//     let (tree, _) = parse(src);
//     insta::assert_debug_snapshot!(tree.expect("Tree is empty"));
// }

// #[test]
// fn parse_string() {
//     let src = "\"Hello, World!\"";
//     let (tree, _) = parse(src);
//     insta::assert_debug_snapshot!(tree.expect("Tree is empty"));
// }

// #[test]
// fn parse_char() {
//     let src = "'a'";
//     let (tree, _) = parse(src);
//     insta::assert_debug_snapshot!(tree.expect("Tree is empty"));
// }

// #[test]
// fn parse_bool() {
//     let src = "true";
//     let (tree, _) = parse(src);
//     insta::assert_debug_snapshot!(tree.expect("Tree is empty"));
// }

// #[test]
// fn parse_list() {
//     let src = "[1, 2, 3]";
//     let (tree, _) = parse(src);
//     insta::assert_debug_snapshot!(tree.expect("Tree is empty"));
// }

// #[test]
// fn parse_tuple() {
//     let src = "(1, 2, 3)";
//     let (tree, _) = parse(src);
//     insta::assert_debug_snapshot!(tree.expect("Tree is empty"));
// }

// #[test]
// fn parse_lambda() {
//     let input = "\\x -> 1";
//     let ast = Parser::new(input).item().expect("failed to parse");
//     insta::assert_debug_snapshot!(ast);
// }

// #[test]
// fn parse_if() {
//     let src = "if a then b else c";
//     let (tree, _) = parse(src);
//     insta::assert_debug_snapshot!(tree);
// }

// #[test]
// fn parse_elif() {
//     let src = "if a then b elif c then d else e";
//     let (tree, _) = parse(src);
//     insta::assert_debug_snapshot!(tree);
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
