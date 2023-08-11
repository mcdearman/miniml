use crate::error::ParserError;

use super::error::SyntaxError;
use itertools::join;
use logos::Logos;
use miniml_util::{
    intern::InternedString,
    span::{Span, Spannable, Spanned},
};
use num_complex::Complex;
use num_rational::Rational64;
use std::{
    fmt::{Debug, Display},
    iter::Peekable,
    vec::IntoIter,
};

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum TokenKind {
    Eof,
    #[regex("[ \n\t\r]+", logos::skip)]
    Whitespace,
    #[regex(r#"--[^\n]*|/\*([^*]|\**[^*/])*\*+/"#)]
    Comment,
    #[regex(r##"([A-Za-z]|_)([A-Za-z]|_|\d)*"##)]
    Ident,
    #[regex(
        r#"-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))"#,
        priority = 2
    )]
    Int,
    #[regex(r#"\d+/\d+"#, priority = 1)]
    Rational,
    #[regex(r#"((\d+(\.\d+))|(\.\d+))([Ee](\+|-)?\d+)?"#, priority = 1)]
    Real,
    #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?i"#, priority = 0)]
    Imag,
    #[regex(r#"'\w'"#)]
    Char,
    #[regex(r#""((\\"|\\\\)|[^\\"])*""#)]
    String,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("^")]
    Caret,

    #[token("\\")]
    Backslash,
    #[token("->")]
    Arrow,
    #[token("=>")]
    FatArrow,
    #[token("|")]
    Pipe,
    #[token("|>")]
    PipeArrow,

    #[token("=")]
    Eq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("!=")]
    Neq,
    #[token("<=")]
    Leq,
    #[token(">=")]
    Geq,

    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("[")]
    LBrack,
    #[token("]")]
    RBrack,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token(",")]
    Comma,
    #[token(".")]
    Period,
    #[token(";")]
    Semicolon,
    #[token(":")]
    Colon,

    #[token("pub")]
    Pub,
    #[token("mod")]
    Module,
    #[token("end")]
    End,
    #[token("use")]
    Use,
    #[token("const")]
    Const,
    #[token("let")]
    Let,
    #[token("fn")]
    Fn,
    #[token("struct")]
    Struct,
    #[token("match")]
    Match,
    #[token("with")]
    With,
    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("not")]
    Not,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("elif")]
    Elif,
    #[token("else")]
    Else,
    #[token("in")]
    In,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TokenKind::Eof => "<EOF>",
                TokenKind::Whitespace => "<WS>",
                TokenKind::Comment => "Comment",
                TokenKind::Ident => "Ident",
                TokenKind::Int => "Int",
                TokenKind::Rational => "Rational",
                TokenKind::Real => "Real",
                TokenKind::Imag => "Imag",
                TokenKind::Char => "Char",
                TokenKind::String => "String",
                TokenKind::Plus => "+",
                TokenKind::Minus => "-",
                TokenKind::Star => "*",
                TokenKind::Slash => "/",
                TokenKind::Percent => "%",
                TokenKind::Caret => "^",
                TokenKind::Backslash => "\\",
                TokenKind::Arrow => "->",
                TokenKind::FatArrow => "=>",
                TokenKind::Pipe => "|",
                TokenKind::PipeArrow => "|>",
                TokenKind::Eq => "=",
                TokenKind::Lt => "<",
                TokenKind::Gt => ">",
                TokenKind::Neq => "!=",
                TokenKind::Leq => "<=",
                TokenKind::Geq => ">=",
                TokenKind::LParen => "(",
                TokenKind::RParen => ")",
                TokenKind::LBrack => "[",
                TokenKind::RBrack => "]",
                TokenKind::LBrace => "{",
                TokenKind::RBrace => "}",
                TokenKind::Comma => ",",
                TokenKind::Period => ".",
                TokenKind::Semicolon => ";",
                TokenKind::Colon => ":",
                TokenKind::Pub => "pub",
                TokenKind::Module => "mod",
                TokenKind::End => "end",
                TokenKind::Use => "use",
                TokenKind::Const => "const",
                TokenKind::Let => "let",
                TokenKind::Fn => "fn",
                TokenKind::Struct => "struct",
                TokenKind::Match => "match",
                TokenKind::With => "with",
                TokenKind::And => "and",
                TokenKind::Or => "or",
                TokenKind::Not => "not",
                TokenKind::If => "if",
                TokenKind::Then => "then",
                TokenKind::Elif => "elif",
                TokenKind::Else => "else",
                TokenKind::In => "in",
            }
        )
    }
}

pub type Token = Spanned<TokenKind>;

#[derive(Debug, Clone)]
pub struct TokenStream<I: Iterator<Item = Token> + Clone> {
    tokens: Peekable<I>,
}

impl<I: Iterator<Item = Token> + Clone> TokenStream<I> {
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
        }
    }

    fn eof_span(&self) -> Span {
        let eofs = self
            .tokens
            .clone()
            .last()
            .unwrap_or(TokenKind::Eof.spanned(Span::new(0, 0)))
            .span
            .end
            + 1;
        Span::new(eofs, eofs)
    }

    pub fn peek(&mut self) -> Token {
        let eofs = self.eof_span();
        self.tokens
            .peek()
            .unwrap_or(&TokenKind::Eof.spanned(eofs))
            .clone()
    }

    pub fn next(&mut self) -> Token {
        self.tokens
            .next()
            .unwrap_or(TokenKind::Eof.spanned(self.eof_span()))
    }

    pub fn at(&mut self, token: TokenKind) -> bool {
        self.peek().value == token.clone()
    }

    pub fn eat(&mut self, token: TokenKind) -> Result<(), Spanned<SyntaxError>> {
        if self.at(token) {
            self.next();
            Ok(())
        } else {
            Err(SyntaxError::UnexpectedToken(self.peek()).spanned(self.peek().span))
        }
    }
}

impl<I: Iterator<Item = Token> + Clone> Display for TokenStream<I> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            join(
                self.tokens
                    .clone()
                    .map(|spnd| format!("{} - {}", spnd.value, spnd.span)),
                "\n"
            )
        )
    }
}

pub fn lex<'src>(src: &'src str) -> (TokenStream<IntoIter<Token>>, Vec<ParserError>) {
    let (tokens, errors): (
        Vec<Option<Spanned<TokenKind>>>,
        Vec<Option<Spanned<SyntaxError>>>,
    ) = TokenKind::lexer(src)
        .spanned()
        .map(|(res, span)| match res {
            Ok(t) => (Some(t.spanned(Span::from(span))), None),
            Err(_) => (
                None,
                Some(SyntaxError::LexerError.spanned(Span::from(span))),
            ),
        })
        .unzip();

    (
        TokenStream::new(tokens.into_iter().flatten().collect::<Vec<_>>().into_iter()),
        errors.into_iter().flatten().collect(),
    )
}

// #[cfg(test)]
// mod tests {
//     #[test]
//     fn lex_int() {
//         let src = "-523";
//         let tokens = TokenKindKind::lexer(src)
//             .spanned()
//             .map(|(t, s)| TokenKind {
//                 kind: t.map_err(|e| panic!("{:?}", e)).unwrap(),
//                 span: s.into(),
//             })
//             .collect::<Vec<_>>();
//         let token_lits = tokens
//             .iter()
//             .map(|t| (t.clone(), src[t.span].to_string()))
//             .collect::<Vec<_>>();
//         insta::assert_debug_snapshot!(token_lits);
//     }
// }
