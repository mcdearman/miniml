use super::token::{Token, TokenKind};
use logos::Logos;
use std::fmt::Debug;

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    logos: logos::Lexer<'src, TokenKind>,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            logos: TokenKind::lexer(src),
        }
    }
}

impl Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.logos.next().map(|r| match r {
            Ok(k) => Token::new(k, self.logos.span().into()),
            Err(_) => Token::new(TokenKind::Error, self.logos.span().into()),
        })
    }
}

mod tests {
    use super::Lexer;
    use itertools::Itertools;

    #[test]
    fn test_lex_fib() {
        let src = r#"
            (let (fib n)
                (if (<= n 1)
                    n
                    (+ (fib (- n 1)) (fib (- n 2)))))
        "#;
        let lexer = Lexer::new(src);
        insta::assert_debug_snapshot!(lexer.collect_vec());
    }
}
