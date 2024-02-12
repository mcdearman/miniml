use self::ast::*;
use crate::{
    lex::token::Token,
    utils::{intern::InternedString, span::Span},
};
use chumsky::{
    error::Rich, extra, input::{Input, Stream, ValueInput}, pratt::prefix, primitive::just, recursive::recursive, select, IterParser, Parser as ChumskyParser
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
        .map_with(|decls, e| Root::new(decls, e.span()))
        .boxed()
}

fn repl_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Root, extra::Err<Rich<'a, Token, Span>>> {
    expr_parser()
        .map(|e| DeclKind::Let {
            name: Ident::new(InternedString::from("main"), e.span().clone()),
            expr: Expr::new(
                ExprKind::Lambda {
                    params: vec![Ident::new(InternedString::from("args"), e.span().clone())],
                    expr: e.clone(),
                },
                e.span().clone(),
            ),
        })
        .map_with(|kind, e| Decl::new(kind, e.span()))
        .or(decl_parser())
        .map_with(|decl, e| Root::new(vec![decl], e.span()))
        .boxed()
}

fn decl_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Decl, extra::Err<Rich<'a, Token, Span>>> {
    let let_ = just(Token::Let)
        .ignore_then(ident_parser())
        .then_ignore(just(Token::Eq))
        .then(expr_parser())
        .map(|(name, expr)| DeclKind::Let { name, expr });

    let fn_ = just(Token::Let)
        .ignore_then(ident_parser())
        .then(ident_parser().repeated().at_least(1).collect())
        .then_ignore(just(Token::Eq))
        .then(expr_parser())
        .map(|((name, params), expr)| DeclKind::Let {
            name: name.clone(),
            expr: Expr::new(
                ExprKind::Lambda {
                    params,
                    expr: expr.clone(),
                },
                name.span().extend(*expr.span()),
            ),
        });

    let_.or(fn_).map_with(|kind, e| Decl::new(kind, e.span()))
}

fn expr_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Expr, extra::Err<Rich<'a, Token, Span>>> {
    recursive(|expr| {
        let unit = just(Token::LParen)
            .ignore_then(just(Token::RParen))
            .map(|_| ExprKind::Unit);

        let lit = lit_parser().map(|lit| ExprKind::Lit(lit));

        let ident = ident_parser().map(|ident| ExprKind::Ident(ident));

        let lambda = just(Token::Backslash)
            .ignore_then(ident_parser().repeated().at_least(1).collect())
            .then_ignore(just(Token::RArrow))
            .then(expr.clone())
            .map(|(params, expr)| ExprKind::Lambda { params, expr });

        let if_ = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .then_ignore(just(Token::Else))
            .then(expr.clone())
            .map(|((cond, then), else_)| ExprKind::If { cond, then, else_ });

        let let_ = just(Token::Let)
            .ignore_then(ident_parser())
            .then_ignore(just(Token::Eq))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((name, expr), body)| ExprKind::Let { name, expr, body });

        let atom = unit
            .or(lit)
            .or(ident)
            .or(lambda)
            .or(if_)
            .or(let_)
            .map_with(|kind, e| Expr::new(kind, e.span()))
            .or(just(Token::LParen)
                .ignore_then(expr)
                .then_ignore(just(Token::RParen)))
            .boxed();

        let apply = atom
            .clone()
            .then(atom.repeated().at_least(1).collect().or_not())
            .map(|(fun, args)| match args {
                Some(args) => ExprKind::Apply { fun, args },
                None => fun.kind().clone(),
            })
            .map_with(|kind, e| Expr::new(kind, e.span()));

        let ops = atom.pratt((
            prefix(2, just(Token::Minus).map(BinaryOp::from), fold)
        ));

        apply
    })
}

fn ident_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Ident, extra::Err<Rich<'a, Token, Span>>> {
    select! {
        Token::Ident(name) => name
    }
    .map_with(|name, e| Ident::new(name, e.span()))
    // .map_with_span(|name, span| Ident::new(name, span))
}

fn lit_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Lit, extra::Err<Rich<'a, Token, Span>>> {
    select! {
        Token::Int(i) => Lit::Int(i),
        // Token::Real(r) => Lit::Real(r),
        // Token::Rational(r) => Lit::Rational(r),
        Token::Bool(b) => Lit::Bool(b),
        // Token::String(s) => Lit::String(s),
        // Token::Char(c) => Lit::Char(c),
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
