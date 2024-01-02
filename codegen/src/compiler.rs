use crate::lexer::Lexer;

#[derive(Debug)]
pub struct Compiler<'src> {
    src: &'src str,
    lexer: Lexer<'src>,
}

impl<'src> Compiler<'src> {
    pub fn new(src: &'src str) -> Self {
        Self {
            src,
            lexer: Lexer::new(src),
        }
    }

    pub fn run(&mut self) {
        for token in &mut self.lexer {
            println!("{:?}", token);
        }
    }

    // pub fn compile(&mut self) -> Result<Chunk, CompileError> {
    //     let mut lexer = Lexer::new(self.src);
    //     let mut parser = Parser::new(&mut lexer);
    //     let ast = parser.parse()?;
    //     let mut codegen = Codegen::new();
    //     codegen.gen(ast)?;
    //     Ok(codegen.chunk)
    // }
}
