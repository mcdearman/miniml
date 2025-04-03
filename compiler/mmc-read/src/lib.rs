use mmc_token_tree::TokenTree;
use mmc_tokenize::token_stream::{self, TokenStream};
use reader_error::ReaderError;

pub mod reader_error;

#[derive(Debug)]
pub struct Reader<'src> {
    token_stream: TokenStream<'src>,
}

impl<'src> Reader<'src> {
    pub fn new(token_stream: TokenStream<'src>) -> Self {
        Self { token_stream }
    }

    pub fn read(&mut self) -> (TokenTree, Vec<ReaderError>) {
        todo!()
    }
}
