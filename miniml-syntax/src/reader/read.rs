use super::{lexer::Lexer, sexpr::*, token::TokenKind};
use chumsky::{
    extra,
    input::{Stream, ValueInput},
    prelude::{Input, Rich},
    primitive::just,
    recursive::recursive,
    select, IterParser, Parser,
};
use miniml_common::{span::Span, symbol::Symbol};

pub type ParseError<'a> = Rich<'a, TokenKind, Span, &'a str>;

pub fn read<'src>(src: &'src str) -> Result<Root, Vec<Rich<'src, TokenKind, Span>>> {
    let lexer = Lexer::new(src);
    let tok_stream = Stream::from_iter(lexer).spanned(Span::from(src.len()..src.len()));
    root_reader().parse(tok_stream).into_result()
}

fn root_reader<'a, I: ValueInput<'a, Token = TokenKind, Span = Span>>(
) -> impl Parser<'a, I, Root, extra::Err<Rich<'a, TokenKind, Span>>> {
    sexpr_reader()
        .repeated()
        .collect()
        .map_with_span(Root::new)
        .boxed()
}

fn sexpr_reader<'a, I: ValueInput<'a, Token = TokenKind, Span = Span>>(
) -> impl Parser<'a, I, Sexpr, extra::Err<Rich<'a, TokenKind, Span>>> {
    recursive(|sexpr| {
        let atom = symbol_reader()
            .map(AtomKind::Sym)
            .or(lit_reader().map(AtomKind::Lit))
            .map_with_span(Atom::new)
            .map(SexprKind::Atom)
            .map_with_span(Sexpr::new)
            .boxed();

        let list = sexpr
            .clone()
            .repeated()
            .at_least(1)
            .collect::<Vec<_>>()
            .map(List::from)
            .map(SexprKind::List)
            .map_with_span(Sexpr::new)
            .delimited_by(just(TokenKind::LParen), just(TokenKind::RParen));

        let empty = just(TokenKind::LParen)
            .then(just(TokenKind::RParen))
            .map_with_span(|_, span| span)
            .map(|_| SexprKind::List(List::Empty))
            .map_with_span(Sexpr::new);

        let vector = sexpr
            .clone()
            .repeated()
            .collect()
            .map(SexprKind::Vector)
            .map_with_span(Sexpr::new)
            .delimited_by(just(TokenKind::LBrack), just(TokenKind::RBrack));

        // quote = "'" sexpr
        let quote = just(TokenKind::Quote)
            .map_with_span(|_, span| span)
            .then(sexpr.clone())
            .map(|(span, sexpr)| {
                // match sexpr.kind() {
                //     SexprKind::List(l) => match l {
                //         List::Empty => {

                //         _ => {}
                //     },
                //     _ => {}
                // }
                let mut list = List::Empty;
                list.push_front(sexpr);
                list.push_front(Sexpr::new(
                    SexprKind::Atom(Atom::new(AtomKind::Sym(Symbol::from("quote")), span)),
                    span,
                ));
                SexprKind::List(list)
            })
            .map_with_span(Sexpr::new);

        let quasiquote = just(TokenKind::Backquote)
            .map_with_span(|_, span| span)
            .then(sexpr.clone())
            .map(|(span, sexpr)| {
                let mut list = List::Empty;
                list.push_front(sexpr);
                list.push_front(Sexpr::new(
                    SexprKind::Atom(Atom::new(AtomKind::Sym(Symbol::from("quasiquote")), span)),
                    span,
                ));
                SexprKind::List(list)
            })
            .map_with_span(Sexpr::new);

        let unquote = just(TokenKind::Comma)
            .map_with_span(|_, span| span)
            .then(sexpr.clone())
            .map(|(span, sexpr)| {
                let mut list = List::Empty;
                list.push_front(sexpr);
                list.push_front(Sexpr::new(
                    SexprKind::Atom(Atom::new(AtomKind::Sym(Symbol::from("unquote")), span)),
                    span,
                ));
                SexprKind::List(list)
            })
            .map_with_span(Sexpr::new);

        let unquote_splice = just(TokenKind::CommaAt)
            .map_with_span(|_, span| span)
            .then(sexpr.clone())
            .map(|(span, sexpr)| {
                let mut list = List::Empty;
                list.push_front(sexpr);
                list.push_front(Sexpr::new(
                    SexprKind::Atom(Atom::new(
                        AtomKind::Sym(Symbol::from("unquote-splicing")),
                        span,
                    )),
                    span,
                ));
                SexprKind::List(list)
            })
            .map_with_span(Sexpr::new);

        atom.or(list)
            .or(empty)
            .or(vector)
            .or(quote)
            .or(quasiquote)
            .or(unquote)
            .or(unquote_splice)
    })
}

fn symbol_reader<'a, I: ValueInput<'a, Token = TokenKind, Span = Span>>(
) -> impl Parser<'a, I, Symbol, extra::Err<Rich<'a, TokenKind, Span>>> {
    select! {
        TokenKind::Symbol(name) => name,
    }
}

fn lit_reader<'a, I: ValueInput<'a, Token = TokenKind, Span = Span>>(
) -> impl Parser<'a, I, Lit, extra::Err<Rich<'a, TokenKind, Span>>> {
    select! {
        TokenKind::Number(n) => Lit::Num(n),
        TokenKind::String(s) => Lit::Str(s),
    }
}
