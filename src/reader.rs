// use super::{
//     sexpr::{Atom, Root, Sexpr},
//     token::Token,
// };
// use crate::util::{intern::InternedString, meta::Meta, span::Span};
// use chumsky::{
//     extra,
//     input::{Stream, ValueInput},
//     prelude::{Input, Rich},
//     primitive::just,
//     recursive::{recursive, Recursive},
//     select, IterParser, Parser,
// };
// use logos::Logos;

// pub type ReadError<'a> = Rich<'a, Token, Span>;

// fn sexpr_reader<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
// ) -> impl Parser<'a, I, Sexpr, extra::Err<Rich<'a, Token, Span>>> {
//     recursive(
//         |sexpr: Recursive<dyn Parser<'a, I, Sexpr, extra::Err<Rich<'a, Token, Span>>>>| {
//             let atom = select! {
//                 Token::Ident(name) => Atom::Symbol(name),
//                 Token::Num(n) => Atom::Num(n),
//                 Token::String(s) => Atom::String(s),
//             }
//             .map_with_span(|a, span| Sexpr::Atom {
//                 value: a,
//                 meta: Meta { span },
//             });

//             let quote = just(Token::Quote)
//                 .map_with_span(|_, span: Span| span)
//                 .then(sexpr.clone())
//                 .map(|(span, sexpr)| {
//                     let quote = Sexpr::Atom {
//                         value: Atom::Symbol(InternedString::from("quote")),
//                         meta: Meta { span },
//                     };
//                     Sexpr::List {
//                         values: vec![quote, sexpr.clone()],
//                         meta: Meta {
//                             span: span.extend(sexpr.span()),
//                         },
//                     }
//                 });

//             let quasiquote = just(Token::Backquote)
//                 .map_with_span(|_, span: Span| span)
//                 .then(sexpr.clone())
//                 .map(|(span, sexpr)| {
//                     let quasi = Sexpr::Atom {
//                         value: Atom::Symbol(InternedString::from("quasiquote")),
//                         meta: Meta { span },
//                     };
//                     Sexpr::List {
//                         values: vec![quasi, sexpr.clone()],
//                         meta: Meta {
//                             span: span.extend(sexpr.span()),
//                         },
//                     }
//                 });

//             let unquote = just(Token::Comma)
//                 .map_with_span(|_, span: Span| span)
//                 .then(sexpr.clone())
//                 .map(|(span, sexpr): (Span, Sexpr)| {
//                     let unquote = Sexpr::Atom {
//                         value: Atom::Symbol(InternedString::from("unquote")),
//                         meta: Meta { span },
//                     };
//                     Sexpr::List {
//                         values: vec![unquote, sexpr.clone()],
//                         meta: Meta {
//                             span: span.extend(sexpr.span()),
//                         },
//                     }
//                 });

//             let unquote_splice = just(Token::CommaAt)
//                 .map_with_span(|_, span: Span| span)
//                 .then(sexpr.clone())
//                 .map(|(span, sexpr)| {
//                     let unquote = Sexpr::Atom {
//                         value: Atom::Symbol(InternedString::from("unquote-splice")),
//                         meta: Meta { span },
//                     };
//                     Sexpr::List {
//                         values: vec![unquote, sexpr.clone()],
//                         meta: Meta {
//                             span: span.extend(sexpr.span()),
//                         },
//                     }
//                 });

//             let dot = sexpr
//                 .clone()
//                 .repeated()
//                 .at_least(1)
//                 .collect::<Vec<_>>()
//                 .then_ignore(just(Token::Period))
//                 .then(sexpr.clone())
//                 .map(|(values, tail)| {
//                     if values.len() == 1 {
//                         Sexpr::Pair {
//                             head: Box::new(values.first().unwrap().clone()),
//                             tail: Box::new(tail.clone()),
//                             meta: Meta {
//                                 span: values.first().unwrap().span().extend(tail.span()),
//                             },
//                         }
//                     } else {
//                         let list = Sexpr::List {
//                             values: values.clone(),
//                             meta: Meta {
//                                 span: values
//                                     .first()
//                                     .unwrap()
//                                     .span()
//                                     .extend(values.last().unwrap().span()),
//                             },
//                         };
//                         Sexpr::Pair {
//                             head: Box::new(list.clone()),
//                             tail: Box::new(tail.clone()),
//                             meta: Meta {
//                                 span: list.span().extend(tail.span()),
//                             },
//                         }
//                     }
//                 })
//                 .delimited_by(just(Token::LParen), just(Token::RParen));

//             let list = sexpr
//                 .repeated()
//                 .at_least(1)
//                 .collect::<Vec<_>>()
//                 .map(|values: Vec<Sexpr>| Sexpr::List {
//                     values: values.clone(),
//                     meta: Meta {
//                         span: values
//                             .first()
//                             .unwrap()
//                             .span()
//                             .extend(values.last().unwrap().span()),
//                     },
//                 })
//                 .delimited_by(just(Token::LParen), just(Token::RParen));

//             atom.or(list)
//                 .or(quote)
//                 .or(quasiquote)
//                 .or(unquote)
//                 .or(unquote_splice)
//                 .or(dot)
//                 .boxed()
//         },
//     )
// }

// fn reader<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
// ) -> impl Parser<'a, I, Root, extra::Err<Rich<'a, Token, Span>>> {
//     sexpr_reader()
//         .repeated()
//         .collect::<Vec<_>>()
//         .map_with_span(|sexprs, span| Root {
//             sexprs,
//             meta: Meta { span },
//         })
// }

// pub fn read<'src>(src: &'src str) -> (Option<Root>, Vec<ReadError<'src>>) {
//     let tokens = Token::lexer(&src).spanned().map(|(tok, span)| match tok {
//         Ok(tok) => (tok, Span::from(span)),
//         Err(err) => panic!("lex error: {:?}", err),
//     });
//     let tok_stream = Stream::from_iter(tokens).spanned(Span::from(src.len()..src.len()));
//     reader().parse(tok_stream).into_output_errors()
// }
