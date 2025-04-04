use mmc_token::{Token, TokenKind};
use mmc_token_tree::TokenTree;
use mmc_tokenize::token_stream::TokenStream;
use reader_error::ReaderError;

pub mod layout;
pub mod reader_error;

#[derive(Debug)]
pub struct Reader<'src> {
    src: &'src str,
    token_stream: TokenStream<'src>,
}

impl<'src> Reader<'src> {
    pub fn new(src: &'src str, token_stream: TokenStream<'src>) -> Self {
        Self { src, token_stream }
    }

    #[inline]
    fn peek(&mut self) -> Token {
        self.token_stream.peek()
    }

    #[inline]
    fn at(&mut self, kind: TokenKind) -> bool {
        *self.peek().kind() == kind
    }

    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.bump();
            true
        } else {
            self.bump();
            false
        }
    }

    pub fn read(&mut self) -> (TokenTree, Vec<ReaderError>) {
        todo!()
    }
}
