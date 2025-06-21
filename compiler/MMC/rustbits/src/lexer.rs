#[derive(Debug, Clone)]
pub struct Lexer {
    logos: logos::Lexer<'static, token::TokKind>,
}

impl Lexer {
    pub fn new(src: &'static str) -> Self {
        let logos = token::TokKind::lexer(src);
        Self { logos }
    }

    pub fn next(&mut self) -> Option<token::Token> {
        self.logos.next().map(|kind| {
            let span = self.logos.span();
            token::Token::new(kind, span)
        })
    }

    pub fn span(&self) -> std::ops::Range<usize> {
        self.logos.span()
    }
}
