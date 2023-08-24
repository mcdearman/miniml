use crate::token::{Token, TokenKind};
use miniml_util::{intern::InternedString, span::Spanned};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum SyntaxError {
    LexerError,
    UnexpectedToken(TokenKind),
    InvalidIntLit(Token),
    InvalidRationalLit(Token),
    InvalidRealLit(Token),
    InvalidComplexLit(Token),
    InvalidCharLit(Token),
    InvalidStringLit(Token),
    UnexpectedEof,
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxError::LexerError => write!(f, "Lexer error"),
            SyntaxError::UnexpectedToken(token) => write!(f, "Unexpected token: {}", token),
            SyntaxError::InvalidIntLit(t) => write!(f, "Invalid integer literal: {}", t),
            SyntaxError::InvalidRationalLit(t) => write!(f, "Invalid rational literal: {}", t),
            SyntaxError::InvalidRealLit(t) => write!(f, "Invalid real literal: {}", t),
            SyntaxError::InvalidComplexLit(t) => write!(f, "Invalid complex literal: {}", t),
            SyntaxError::InvalidCharLit(t) => write!(f, "Invalid character literal: {}", t),
            SyntaxError::InvalidStringLit(t) => write!(f, "Invalid string literal: {}", t),
            SyntaxError::UnexpectedEof => write!(f, "Unexpected end of file"),
        }
    }
}

pub type ParserError = Spanned<SyntaxError>;
pub type ParseResult<T> = Result<T, ParserError>;
