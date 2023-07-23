use crate::lex::Token;
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq)]
pub enum SyntaxError {
    LexerError,
    UnexpectedToken(Token),
    UnexpectedEof,
}

impl Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SyntaxError::LexerError => write!(f, "Lexer error"),
            SyntaxError::UnexpectedToken(token) => write!(f, "Unexpected token: {}", token),
            SyntaxError::UnexpectedEof => write!(f, "Unexpected end of file"),
        }
    }
}
