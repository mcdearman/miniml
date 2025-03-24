#[derive(Debug, Clone, PartialEq)]
pub struct Pipeline<'src> {
    src: &'src str,
}

impl<'src> Pipeline<'src> {
    pub fn new(src: &'src str) -> Self {
        Self { src }
    }

    pub fn run(&self) {
        let stream = TokenStream::new(self.src);
        let (ast, errors) = parse(stream, true);
    }
}
