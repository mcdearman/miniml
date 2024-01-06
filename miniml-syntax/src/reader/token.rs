use logos::Logos;
use miniml_common::{num::Num, span::Span, symbol::Symbol};
use num_rational::Rational64;
use std::fmt::{Debug, Display};

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    kind: TokenKind,
    span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenKind::*;
        match &self.kind {
            Eof => write!(f, "EOF @ {}", self.span),
            Error => write!(f, "Error @ {}", self.span),
            Whitespace => write!(f, "Whitespace @ {}", self.span),
            Comment => write!(f, "Comment @ {}", self.span),
            Ident(name) => write!(f, "Ident({}) @ {}", name, self.span),
            Number(n) => write!(f, "Number({}) @ {}", n, self.span),
            String(s) => write!(f, "String({}) @ {}", s, self.span),
            LParen => write!(f, "( @ {}", self.span),
            RParen => write!(f, ") @ {}", self.span),
            LBrack => write!(f, "[ @ {}", self.span),
            RBrack => write!(f, "] @ {}", self.span),
            LBrace => write!(f, "{{ @ {}", self.span),
            RBrace => write!(f, "}} @ {}", self.span),
            Colon => write!(f, ": @ {}", self.span),
            Period => write!(f, ". @ {}", self.span),
            Comma => write!(f, ", @ {}", self.span),
            CommaAt => write!(f, ",@ @ {}", self.span),
            Hash => write!(f, "# @ {}", self.span),
            Quote => write!(f, "' @ {}", self.span),
            Backquote => write!(f, "` @ {}", self.span),
        }
    }
}

#[derive(Logos, Debug, Copy, Clone, Default, PartialEq)]
pub enum TokenKind {
    Eof,
    #[default]
    Error,
    #[regex(r"[ \t\r\n\f]+", logos::skip)]
    Whitespace,
    #[regex(r#";[^\n]*"#)]
    Comment,
    #[regex(r#"[^'\[\]()\s,{};]+"#, |lex| Symbol::from(lex.slice()))]
    Ident(Symbol),
    #[regex(
        r#"((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))(/-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0)))?"#,
        priority = 2,
        callback = |lex| lex.slice().parse::<Rational64>().ok()
    )]
    Number(Rational64),
    #[regex(r#""("[^"\\]*(?:\\.[^"\\]*)*")""#, |lex| Symbol::from(lex.slice()))]
    String(Symbol),

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
    #[token(":")]
    Colon,
    #[token(".")]
    Period,
    #[token(",")]
    Comma,
    #[token(",@")]
    CommaAt,
    #[token("#")]
    Hash,
    #[token("'")]
    Quote,
    #[token("`")]
    Backquote,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use TokenKind::*;
        match self {
            Eof => write!(f, "EOF"),
            Error => write!(f, "Error"),
            Whitespace => write!(f, "Whitespace"),
            Comment => write!(f, "Comment"),
            Ident(name) => write!(f, "Ident({})", name),
            Number(n) => write!(f, "Number({})", n),
            String(s) => write!(f, "String({:?})", s),
            LParen => write!(f, "("),
            RParen => write!(f, ")"),
            LBrack => write!(f, "["),
            RBrack => write!(f, "]"),
            LBrace => write!(f, "{{"),
            RBrace => write!(f, "}}"),
            Colon => write!(f, ":"),
            Period => write!(f, "."),
            Comma => write!(f, ","),
            CommaAt => write!(f, ",@"),
            Hash => write!(f, "#"),
            Quote => write!(f, "'"),
            Backquote => write!(f, "`"),
        }
    }
}
