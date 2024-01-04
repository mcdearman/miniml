use super::{
    lexer::Lexer,
    sexpr::{Lit, Root, Sexpr},
    token::TokenKind,
};
use chumsky::{
    extra,
    input::{Stream, ValueInput},
    prelude::{Input, Rich},
    primitive::just,
    recursive::recursive,
    select, IterParser, Parser,
};
use miniml_common::{interner::InternedString, span::Span};

pub type ParseError<'a> = Rich<'a, TokenKind, Span, &'a str>;

pub fn parse<'src>(src: &'src str) -> Result<Root, Vec<Rich<'src, TokenKind, Span>>> {
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
}

fn ident_reader<'a, I: ValueInput<'a, Token = TokenKind, Span = Span>>(
) -> impl Parser<'a, I, InternedString, extra::Err<Rich<'a, TokenKind, Span>>> {
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
