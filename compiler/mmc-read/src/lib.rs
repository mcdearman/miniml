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
    keep_trivia: bool,
}

impl<'src> Reader<'src> {
    #[inline(always)]
    pub fn new(src: &'src str, token_stream: TokenStream<'src>, keep_trivia: bool) -> Self {
        Self {
            src,
            token_stream,
            keep_trivia,
        }
    }

    #[inline(always)]
    fn peek(&mut self) -> Token {
        self.token_stream.peek()
    }

    #[inline(always)]
    fn next(&mut self) -> Token {
        self.token_stream.next()
    }

    #[inline(always)]
    fn at(&mut self, kind: TokenKind) -> bool {
        *self.peek().kind() == kind
    }

    #[inline(always)]
    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.at(kind) {
            self.next();
            true
        } else {
            self.next();
            false
        }
    }

    #[inline(always)]
    fn text(&self, token: Token) -> &'src str {
        token.text(self.src)
    }

    pub fn read(&mut self) -> (TokenTree, Vec<ReaderError>) {
        // let mut errors = Vec::new();
        // let mut tokens = Vec::new();

        todo!()
    }
}
