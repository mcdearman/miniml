use super::error::SyntaxError;
use logos::Logos;
use miniml_util::{
    intern::InternedString,
    span::{Span, Spanned},
};
use num_complex::Complex;
use num_rational::Rational64;
use std::{
    fmt::{Debug, Display},
    iter::Peekable,
    vec::IntoIter,
};

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    Eof,
    #[regex("[ \n\t\r]+", logos::skip)]
    Whitespace,
    #[regex(r#"--[^\n]*|/\*([^*]|\**[^*/])*\*+/"#)]
    Comment,
    #[regex(r##"([A-Za-z]|_)([A-Za-z]|_|\d)*"##, callback = |lex| InternedString::from(lex.slice()))]
    Ident(InternedString),
    #[regex(
        r#"-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))"#,
        priority = 2,
        callback = |lex| lex.slice().parse().ok()
    )]
    Int(i64),
    #[regex(r#"\d+/\d+"#, priority = 1, callback = |lex| lex.slice().parse().ok())]
    Rational(Rational64),
    #[regex(r#"((\d+(\.\d+))|(\.\d+))([Ee](\+|-)?\d+)?"#, priority = 1, callback = |lex| lex.slice().parse().ok())]
    Real(f64),
    #[regex(r#"((\d+(\.\d+)?)|(\.\d+))([Ee](\+|-)?\d+)?i"#, priority = 0, callback = |lex| lex.slice().parse().ok())]
    Imag(Complex<f64>),
    #[regex(r#"'\w'"#, callback = |lex| lex.slice().parse().ok())]
    Char(char),
    #[regex(r#""((\\"|\\\\)|[^\\"])*""#, callback = |lex| InternedString::from(lex.slice()))]
    String(InternedString),
    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("%")]
    Rem,
    #[token("^")]
    Pow,

    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("\\")]
    Lambda,
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
    Lss,
    #[token(">")]
    Gtr,
    #[token("!")]
    Not,
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
    #[token(";;")]
    DoubleSemicolon,
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

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::Eof => "<EOF>".to_string(),
                Token::Whitespace => "<WS>".to_string(),
                Token::Comment => "Comment".to_string(),
                Token::Ident(_) => "Ident".to_string(),
                Token::Int(i) => i.to_string(),
                Token::Rational(r) => r.to_string(),
                Token::Real(r) => r.to_string(),
                Token::Imag(i) => i.to_string(),
                Token::Char(c) => c.to_string(),
                Token::String(s) => s.to_string(),
                Token::Add => "+".to_string(),
                Token::Sub => "-".to_string(),
                Token::Mul => "*".to_string(),
                Token::Div => "/".to_string(),
                Token::Rem => "%".to_string(),
                Token::Pow => "^".to_string(),
                Token::And => "&&".to_string(),
                Token::Or => "||".to_string(),
                Token::Lambda => "lambda".to_string(),
                Token::Arrow => "->".to_string(),
                Token::FatArrow => "=>".to_string(),
                Token::Pipe => "|".to_string(),
                Token::PipeArrow => "|>".to_string(),
                Token::Eq => "=".to_string(),
                Token::Lss => "<".to_string(),
                Token::Gtr => ">".to_string(),
                Token::Not => "!".to_string(),
                Token::Neq => "!=".to_string(),
                Token::Leq => "<=".to_string(),
                Token::Geq => ">=".to_string(),
                Token::LParen => "(".to_string(),
                Token::RParen => ")".to_string(),
                Token::LBrack => "[".to_string(),
                Token::RBrack => "]".to_string(),
                Token::LBrace => "{".to_string(),
                Token::RBrace => "}".to_string(),
                Token::Comma => ",".to_string(),
                Token::Period => ".".to_string(),
                Token::Semicolon => ";".to_string(),
                Token::DoubleSemicolon => ";;".to_string(),
                Token::Colon => ":".to_string(),
                Token::Pub => "pub".to_string(),
                Token::Module => "mod".to_string(),
                Token::End => "end".to_string(),
                Token::Use => "use".to_string(),
                Token::Let => "let".to_string(),
                Token::Fn => "fn".to_string(),
                Token::Struct => "struct".to_string(),
                Token::Match => "match".to_string(),
                Token::With => "with".to_string(),
                Token::If => "if".to_string(),
                Token::Then => "then".to_string(),
                Token::Elif => "elif".to_string(),
                Token::Else => "else".to_string(),
                Token::In => "in".to_string(),
            }
        )
    }
}

#[derive(Debug, Clone)]
pub struct TokenStream {
    tokens: Peekable<IntoIter<Spanned<Token>>>,
}

impl TokenStream {
    pub fn new(tokens: Vec<Spanned<Token>>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    pub fn peek(&mut self) -> Spanned<Token> {
        self.tokens
            .peek()
            .unwrap_or(&(Token::Eof, Span::new(0, 0)))
            .clone()
    }

    pub fn next(&mut self) -> Spanned<Token> {
        self.tokens.next().unwrap_or((Token::Eof, Span::new(0, 0)))
    }

    pub fn at(&mut self, token: &Token) -> bool {
        self.peek().0 == token.clone()
    }

    pub fn eat(&mut self, token: &Token) -> bool {
        if self.at(token) {
            self.next();
            true
        } else {
            false
        }
    }
}

pub fn lex<'src>(src: &'src str) -> Result<TokenStream, Vec<Spanned<SyntaxError>>> {
    let (tokens, errors): (
        Vec<Option<Spanned<Token>>>,
        Vec<Option<Spanned<SyntaxError>>>,
    ) = Token::lexer(src)
        .spanned()
        .map(|(res, span)| match res {
            Ok(t) => (Some((t, Span::from(span))), None),
            Err(_) => (None, Some((SyntaxError::LexerError, Span::from(span)))),
        })
        .unzip();

    if errors.iter().any(|e| e.is_some()) {
        Err(errors.into_iter().flatten().collect())
    } else {
        Ok(TokenStream::new(
            tokens.into_iter().flatten().collect::<Vec<_>>(),
        ))
    }
}

// #[cfg(test)]
// mod tests {
//     #[test]
//     fn lex_int() {
//         let src = "-523";
//         let tokens = TokenKind::lexer(src)
//             .spanned()
//             .map(|(t, s)| Token {
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
