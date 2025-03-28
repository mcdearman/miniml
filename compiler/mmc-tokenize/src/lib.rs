use token_stream::TokenStream;

pub mod token_stream;

#[inline]
pub fn tokenize<'src>(src: &'src str) -> TokenStream<'src> {
    TokenStream::new(src)
}
