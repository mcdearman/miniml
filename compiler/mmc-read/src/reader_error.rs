use mmc_token::Token;
use mmc_utils::{intern::InternedString, span::Span};

#[derive(Debug, Clone)]
pub struct ReaderError {
    pub kind: ReaderErrorKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum ReaderErrorKind {
    UnexpectedEof,
    UnexpectedToken(Token),
    InvalidCharacter(char),
    InvalidString(InternedString),
    InvalidNumber(InternedString),
    InvalidIdentifier(InternedString),
    InvalidComment(InternedString),
    InvalidDelimiter(InternedString),
}
