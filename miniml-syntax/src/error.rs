use crate::lex::{Token, TokenKind};
use miniml_util::span::Spanned;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum SyntaxError {
    LexerError,
    UnexpectedToken(TokenKind),
    LitParseError,
    UnexpectedEof,
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxError::LexerError => write!(f, "Lexer error"),
            SyntaxError::UnexpectedToken(token) => write!(f, "Unexpected token: {}", token),
            SyntaxError::LitParseError => write!(f, "Literal parse error"),
            SyntaxError::UnexpectedEof => write!(f, "Unexpected end of file"),
        }
    }
}

pub type ParserError = Spanned<SyntaxError>;
pub type ParseResult<T> = Result<T, ParserError>;
