use mmc_tokenize::tokenize;

#[derive(Debug, Clone, PartialEq)]
pub struct Pipeline<'src> {
    src: &'src str,
}

impl<'src> Pipeline<'src> {
    pub fn new(src: &'src str) -> Self {
        Self { src }
    }

    pub fn run(&self) {
        let mut token_stream = tokenize(self.src);
        println!("{:#?}", token_stream.collect_tokens());
        // let (ast, errors) = parse(stream, true);
    }
}

// let mut res = Resolver::new();
// let mut solver = TypeSolver::new();
// let stream = TokenStream::new(&line);

// match parse(stream, true) {
//     (Some(ast), _) => {
//         // log::debug!("AST: {:#?}", ast);
//         match res.resolve(&ast) {
//             (Some(nir), errors) => {
//                 if !errors.is_empty() {
//                     // log::error!("Resolution errors: {:#?}", errors);
//                     eprint!("Resolution errors: {:#?}", errors);
//                     res.clear_errors();
//                     continue;
//                 }
//                 // log::debug!("NIR: {:#?}", nir);
//                 // println!("NIR: {:#?}", nir);
//                 let mut scc_ctx = scc::Context::new();
//                 let sir = scc_ctx.run(&nir);
//                 // log::debug!("SCC: {:#?}", sir);
//                 // println!("SCC: {:#?}", sir);
//                 let (tir, errors) = solver.infer(&*line, &sir);
//                 if !errors.is_empty() {
//                     // log::error!("Inference errors: {:#?}", errors);
//                     eprint!("Inference errors: {:#?}", errors);
//                     continue;
//                 }
//                 println!("TIR: {:#?}", tir);
//             }
//             (None, res_errors) => {
//                 // log::error!("Resolution errors: {:#?}", res_errors);
//                 eprint!("Resolution errors: {:#?}", res_errors);
//                 continue;
//             }
//         }
//     }
//     (None, parse_errors) => {
//         // log::error!("Parse errors: {:#?}", parse_errors);
//         eprint!("Parse errors: {:#?}", parse_errors);
//         continue;
//     }
// };
