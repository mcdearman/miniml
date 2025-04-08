use mmc_token_tree::Delim;
use mmc_utils::span::Span;

#[derive(Debug, Clone)]
pub struct ReaderError {
    delim: Delim,
    span: Span,
}

impl ReaderError {
    pub fn new(delim: Delim, span: Span) -> Self {
        Self { delim, span }
    }

    pub fn delim(&self) -> &Delim {
        &self.delim
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}
