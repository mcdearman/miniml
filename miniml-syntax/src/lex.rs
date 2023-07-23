use super::error::SyntaxError;
use logos::Logos;
use miniml_util::span::{Span, Spanned};
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
    #[regex(r##"([A-Za-z]|_)([A-Za-z]|_|\d)*"##)]
    Ident,
    #[regex(
        r#"(\+|-)*((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))"#,
        priority = 2,
        callback = |lex| lex.slice().parse().ok()
    )]
    Int(i64),
    #[regex(r#"-?\d+/\d+"#, priority = 1)]
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
    #[token("type")]
    Type,
    #[token("match")]
    Match,
    #[token("with")]
    With,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
    #[token("elif")]
    Elif,
    #[token("in")]
    In,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Token::Eof => "<EOF>",
                Token::Whitespace => "<WS>",
                Token::Comment => "Comment",
                Token::Ident => "Ident",
                Token::Int(i) => {
                    let s = &format!("{}", i);
                    s
                }
                Token::Rational => "Rational",
                Token::Real => "Float",
                Token::Imag => "Imag",
                Token::Char => "Char",
                Token::String => "String",
                Token::Add => "+",
                Token::Sub => "-",
                Token::Mul => "*",
                Token::Div => "/",
                Token::Rem => "%",
                Token::Pow => "^",
                Token::And => "&&",
                Token::Or => "||",
                Token::Lambda => "lambda",
                Token::Arrow => "->",
                Token::FatArrow => "=>",
                Token::Pipe => "|",
                Token::PipeArrow => "|>",
                Token::Eq => "=",
                Token::Lss => "<",
                Token::Gtr => ">",
                Token::Not => "!",
                Token::Neq => "!=",
                Token::Leq => "<=",
                Token::Geq => ">=",
                Token::LParen => "(",
                Token::RParen => ")",
                Token::LBrack => "[",
                Token::RBrack => "]",
                Token::LBrace => "{",
                Token::RBrace => "}",
                Token::Comma => ",",
                Token::Period => ".",
                Token::Semicolon => ";",
                Token::DoubleSemicolon => ";;",
                Token::Colon => ":",
                Token::Pub => "pub",
                Token::Module => "mod",
                Token::End => "end",
                Token::Use => "use",
                Token::Let => "let",
                Token::Fn => "fn",
                Token::Type => "type",
                Token::Match => "match",
                Token::With => "with",
                Token::If => "if",
                Token::Then => "then",
                Token::Else => "else",
                Token::Elif => "elif",
                Token::In => "in",
            }
        )
    }
}

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
