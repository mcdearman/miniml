#[derive(Debug, Clone, PartialEq)]
pub struct Pipeline<'src> {
    src: &'src str,
}

impl<'src> Pipeline<'src> {
    pub fn new(src: &'src str) -> Self {
        Self { src }
    }

    pub fn run(&self) -> Result<(), Error> {
        let stream = TokenStream::new(self.src);
        let (ast, _) = parse(stream, true);
        let ast = ast.ok_or_else(|| Error::ParseError)?;
        let ast = rename(ast)?;
        let ast = infer(ast)?;
        Ok(())
    }
}
