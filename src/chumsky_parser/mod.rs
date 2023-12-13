pub mod lexer;
pub mod token;
// use super::{
//     ast::{Expr, InfixOp, Item, Lit, MatchCase, Pattern, PrefixOp, Root},
//     logos_token::Token,
// };
// use chumsky::{
//     extra,
//     input::{Stream, ValueInput},
//     prelude::{Input, Rich},
//     primitive::just,
//     recursive::recursive,
//     select, IterParser, Parser,
// };
// use logos::Logos;

// pub type ParseError<'a> = Rich<'a, Token, Span, &'a str>;

// fn ident_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
// ) -> impl Parser<'a, I, InternedString, extra::Err<Rich<'a, Token, Span>>> {
//     select! {
//         Token::Ident(name) => name,
//     }
// }

// fn lit_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
// ) -> impl Parser<'a, I, Lit, extra::Err<Rich<'a, Token, Span>>> {
//     select! {
//         Token::Num(n) => Lit::Num(n),
//         Token::Bool(b) => Lit::Bool(b),
//         Token::String(s) => Lit::String(s),
//     }
// }

// fn pattern_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
// ) -> impl Parser<'a, I, Pattern, extra::Err<Rich<'a, Token, Span>>> {
//     ident_parser()
//         .map(Pattern::Ident)
//         .or(just(Token::Wildcard).map(|_| Pattern::Wildcard))
//         .or(just(Token::LParen)
//             .then(just(Token::RParen))
//             .map(|_| Pattern::Unit))
//         .or(lit_parser().map(Pattern::Lit))
// }

// fn expr_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
// ) -> impl Parser<'a, I, Expr, extra::Err<Rich<'a, Token, Span>>> {
//     recursive(|expr| {
//         let unit = just(Token::LParen)
//             .ignore_then(just(Token::RParen))
//             .map(|_| Expr::Unit);

//         let lit = lit_parser().map(Expr::Lit);

//         let let_ = just(Token::Let)
//             .ignore_then(pattern_parser().map_with_span(Node::new))
//             .then_ignore(just(Token::Assign))
//             .then(expr.clone().map_with_span(Node::new))
//             .then_ignore(just(Token::In))
//             .then(expr.clone().map_with_span(Node::new))
//             .map(|((pat, expr), body)| Expr::Let { pat, expr, body });

//         let fn_ = just(Token::Let)
//             .ignore_then(ident_parser().map_with_span(Node::new))
//             .then(
//                 pattern_parser()
//                     .map_with_span(Node::new)
//                     .repeated()
//                     .at_least(1)
//                     .collect::<Vec<_>>(),
//             )
//             .then_ignore(just(Token::Assign))
//             .then(expr.clone().map_with_span(Node::new))
//             .then_ignore(just(Token::In))
//             .then(expr.clone().map_with_span(Node::new))
//             .map(|(((name, params), expr), body)| Expr::Fn {
//                 name,
//                 params,
//                 expr,
//                 body,
//             });

//         let lambda = just(Token::Lambda)
//             .ignore_then(
//                 pattern_parser()
//                     .map_with_span(Node::new)
//                     .repeated()
//                     .at_least(1)
//                     .collect::<Vec<_>>(),
//             )
//             .then_ignore(just(Token::Arrow))
//             .then(expr.clone().map_with_span(Node::new))
//             .map(|(params, body)| Expr::Lambda { params, body });

//         // match = "match" expr "with" ("|" case)* "\\" case
//         // case = pat "->" expr
//         let case = pattern_parser()
//             .map_with_span(Node::new)
//             .then_ignore(just(Token::Arrow))
//             .then(expr.clone().map_with_span(Node::new))
//             .map(|(pattern, expr)| MatchCase { pattern, expr })
//             .boxed();

//         let match_ = just(Token::Match)
//             .ignore_then(expr.clone().map_with_span(Node::new))
//             .then_ignore(just(Token::With))
//             .then(
//                 just(Token::Pipe)
//                     .ignore_then(case.clone().map_with_span(Node::new))
//                     .repeated()
//                     .at_least(1)
//                     .collect::<Vec<_>>(),
//             )
//             .map(|(expr, cases)| Expr::Match { expr, cases });

//         let if_ = just(Token::If)
//             .ignore_then(expr.clone().map_with_span(Node::new))
//             .then_ignore(just(Token::Then))
//             .then(expr.clone().map_with_span(Node::new))
//             .then_ignore(just(Token::Else))
//             .then(expr.clone().map_with_span(Node::new))
//             .map(|((cond, then), else_)| Expr::If { cond, then, else_ });

//         // atom = ident | number | bool | '(' expr ')'
//         let atom = ident_parser()
//             .map(Expr::Ident)
//             .or(unit)
//             .or(lit)
//             .or(let_)
//             .or(fn_)
//             .or(match_)
//             .or(if_)
//             .or(lambda)
//             .or(expr
//                 .clone()
//                 .delimited_by(just(Token::LParen), just(Token::RParen)))
//             .map_with_span(Node::new)
//             .boxed();

//         // apply = atom+
//         let apply = atom
//             .clone()
//             .then(atom.clone().repeated().collect::<Vec<_>>())
//             .map(|(fun, args)| {
//                 if args.is_empty() {
//                     fun.clone()
//                 } else {
//                     Node::new(
//                         Expr::Apply {
//                             fun: fun.clone(),
//                             args: args.clone(),
//                         },
//                         Span::new(fun.span().start, args.last().unwrap().span().end),
//                     )
//                 }
//             })
//             .boxed();

//         let op = just(Token::Minus)
//             .map(PrefixOp::from)
//             .map_with_span(Node::new)
//             .or(just(Token::Bang)
//                 .map(PrefixOp::from)
//                 .map_with_span(Node::new))
//             .boxed();

//         // unary = ("-" | "not")* apply
//         let unary = op
//             .clone()
//             .repeated()
//             .foldr(apply.clone(), |op: Node<PrefixOp>, expr: Node<Expr>| {
//                 Node::new(
//                     Expr::Prefix {
//                         op: op.clone(),
//                         expr: expr.clone(),
//                     },
//                     Span::new(op.span().start, expr.span().end),
//                 )
//             })
//             .boxed();

//         let op = just(Token::Star)
//             .map(InfixOp::from)
//             .map_with_span(Node::new)
//             .or(just(Token::Slash)
//                 .map(InfixOp::from)
//                 .map_with_span(Node::new))
//             .or(just(Token::Percent)
//                 .map(InfixOp::from)
//                 .map_with_span(Node::new))
//             .boxed();

//         let factor = unary
//             .clone()
//             .foldl(
//                 op.clone().then(unary.clone()).repeated(),
//                 |lhs: Node<Expr>, (op, rhs): (Node<InfixOp>, Node<Expr>)| {
//                     Node::new(
//                         Expr::Infix {
//                             op,
//                             lhs: lhs.clone(),
//                             rhs: rhs.clone(),
//                         },
//                         Span::new(lhs.span().start, rhs.span().end),
//                     )
//                 },
//             )
//             .boxed();

//         let op = just(Token::Plus)
//             .map(InfixOp::from)
//             .map_with_span(Node::new)
//             .or(just(Token::Minus)
//                 .map(InfixOp::from)
//                 .map_with_span(Node::new))
//             .boxed();

//         let term = factor
//             .clone()
//             .foldl(
//                 op.clone().then(factor.clone()).repeated(),
//                 |lhs: Node<Expr>, (op, rhs): (_, Node<Expr>)| {
//                     Node::new(
//                         Expr::Infix {
//                             op,
//                             lhs: lhs.clone(),
//                             rhs: rhs.clone(),
//                         },
//                         Span::new(lhs.span().start, rhs.span().end),
//                     )
//                 },
//             )
//             .boxed();

//         // cmp = term (('<' | '>' | '<=' | '>=') term)?
//         let cmp = term
//             .clone()
//             .then(
//                 just(Token::Lt)
//                     .map(InfixOp::from)
//                     .map_with_span(Node::new)
//                     .or(just(Token::Gt).map(InfixOp::from).map_with_span(Node::new))
//                     .or(just(Token::Leq).map(InfixOp::from).map_with_span(Node::new))
//                     .or(just(Token::Geq).map(InfixOp::from).map_with_span(Node::new))
//                     .then(term.clone())
//                     .or_not(),
//             )
//             .map(|(lhs, rhs)| match rhs {
//                 Some((op, rhs)) => Node::new(
//                     Expr::Infix {
//                         op,
//                         lhs: lhs.clone(),
//                         rhs: rhs.clone(),
//                     },
//                     Span::new(lhs.span().start, rhs.span().end),
//                 ),
//                 None => lhs.clone(),
//             })
//             .boxed();

//         // eq = cmp (('==' | '!=') cmp)?
//         let eq = cmp
//             .clone()
//             .then(
//                 just(Token::Eq)
//                     .map(InfixOp::from)
//                     .map_with_span(Node::new)
//                     .or(just(Token::Neq).map(InfixOp::from).map_with_span(Node::new))
//                     .then(cmp.clone())
//                     .or_not(),
//             )
//             .map(|(lhs, rhs)| match rhs {
//                 Some((op, rhs)) => Node::new(
//                     Expr::Infix {
//                         op,
//                         lhs: lhs.clone(),
//                         rhs: rhs.clone(),
//                     },
//                     Span::new(lhs.span().start, rhs.span().end),
//                 ),
//                 None => lhs.clone(),
//             })
//             .boxed();

//         eq.map(|expr| expr.inner().clone())
//         // atom.clone()
//     })
// }

// fn fn_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
// ) -> impl Parser<'a, I, Node<Item>, extra::Err<Rich<'a, Token, Span>>> {
//     ident_parser()
//         .map_with_span(Node::new)
//         .then(
//             pattern_parser()
//                 .map_with_span(Node::new)
//                 .repeated()
//                 .at_least(1)
//                 .collect::<Vec<_>>(),
//         )
//         .then_ignore(just(Token::Assign))
//         .then(expr_parser().map_with_span(Node::new))
//         .map(|((name, params), body)| Item::Fn { name, params, body })
//         .map_with_span(Node::new)
//         .boxed()
// }

// fn def_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
// ) -> impl Parser<'a, I, Node<Item>, extra::Err<Rich<'a, Token, Span>>> {
//     pattern_parser()
//         .map_with_span(Node::new)
//         .then_ignore(just(Token::Assign))
//         .then(expr_parser().map_with_span(Node::new))
//         .map(|(pat, expr)| Item::Def { pat, expr })
//         .map_with_span(Node::new)
//         .boxed()
// }

// fn item_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
// ) -> impl Parser<'a, I, Node<Item>, extra::Err<Rich<'a, Token, Span>>> {
//     def_parser()
//         .or(fn_parser())
//         .or(expr_parser().map(Item::Expr).map_with_span(Node::new))
//         .boxed()
// }

// fn parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
// ) -> impl Parser<'a, I, Node<Root>, extra::Err<Rich<'a, Token, Span>>> {
//     item_parser()
//         .repeated()
//         .collect()
//         .map(|items| Root { items })
//         .map_with_span(Node::new)
//         .boxed()
// }

// pub fn parse<'src>(src: &'src str) -> (Option<Node<Root>>, Vec<ParseError<'src>>) {
//     let tokens = Token::lexer(&src).spanned().map(|(tok, span)| match tok {
//         Ok(tok) => (tok, Span::from(span)),
//         Err(err) => panic!("lex error: {:?}", err),
//     });
//     let tok_stream = Stream::from_iter(tokens).spanned(Span::from(src.len()..src.len()));
//     parser().parse(tok_stream).into_output_errors()
// }

// mod tests {
//     use crate::chumsky_parser::parse;

//     fn test_helper(src: &str) -> super::Node<super::Root> {
//         let (root, errs) = parse(src);
//         if !errs.is_empty() {
//             panic!("parse error: {:?}", errs);
//         }
//         root.unwrap()
//     }

//     #[test]
//     fn parse_unit() {
//         insta::assert_debug_snapshot!(test_helper("()"));
//     }

//     #[test]
//     fn parse_num() {
//         insta::assert_debug_snapshot!(test_helper("1"));
//     }

//     #[test]
//     fn parse_bool() {
//         insta::assert_debug_snapshot!(test_helper("true"));
//     }

//     #[test]
//     fn parse_ident() {
//         insta::assert_debug_snapshot!(test_helper("x"));
//     }

//     #[test]
//     fn parse_let() {
//         insta::assert_debug_snapshot!(test_helper("let x = 1 in x"));
//     }

//     #[test]
//     fn parse_let_unary() {
//         insta::assert_debug_snapshot!(test_helper("let x = 1 in -x"));
//     }

//     #[test]
//     fn parse_let_fn() {
//         insta::assert_debug_snapshot!(test_helper("let add x y = x + y in add 1 2"));
//     }

//     #[test]
//     fn parse_apply() {
//         insta::assert_debug_snapshot!(test_helper("f x y"));
//     }

//     #[test]
//     fn parse_prefix() {
//         insta::assert_debug_snapshot!(test_helper("-x"));
//     }

//     #[test]
//     fn parse_infix() {
//         insta::assert_debug_snapshot!(test_helper("x + y * z"));
//     }

//     #[test]
//     fn parse_paren() {
//         insta::assert_debug_snapshot!(test_helper("(x + y) * z"));
//     }

//     #[test]
//     fn parse_lambda() {
//         insta::assert_debug_snapshot!(test_helper("\\a b -> a + b"));
//     }

//     #[test]
//     fn parse_lambda_apply() {
//         insta::assert_debug_snapshot!(test_helper("(\\a b -> a + b) 1 2"));
//     }

//     #[test]
//     fn parse_def() {
//         insta::assert_debug_snapshot!(test_helper("x = 1"));
//     }

//     #[test]
//     fn parse_fn_def() {
//         insta::assert_debug_snapshot!(test_helper("add x y = x + y"));
//     }

//     #[test]
//     fn parse_nested_let() {
//         insta::assert_debug_snapshot!(test_helper("let x = 1 in let y = 2 in x + y"));
//     }
// }
