use super::env::Env;
use crate::{
    analysis::infer::TypeSolver,
    rename::resolver::Resolver,
    utils::{intern::InternedString, scoped_intern::ScopedInterner, unique_id::UniqueId},
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug)]
pub struct Interpreter {
    src: String,
    res: Resolver,
    builtins: HashMap<UniqueId, InternedString>,
    scoped_interner: ScopedInterner,
    type_solver: TypeSolver,
    env: Rc<RefCell<Env>>,
}

impl Interpreter {
    pub fn run() {

        //         let stream = TokenIter::new(&src);
        //         // let (ast, parse_errors) = parse(stream, true);
        //         let ast = match parse(stream, true) {
        //             (Some(root), _) => {
        //                 log::debug!("{:#?}", root);
        //                 root
        //             }
        //             (_, errors) => {
        //                 println!("{:#?}", errors);
        //                 src.clear();
        //                 continue;
        //             }
        //         };

        //         let nir = match res.resolve(&ast) {
        //             (Some(nir), _) => {
        //                 log::debug!("{:#?}", nir);
        //                 nir
        //             }
        //             (_, errors) => {
        //                 println!("{:#?}", errors);
        //                 src.clear();
        //                 continue;
        //             }
        //         };

        //         let tir = match solver.infer(&src, &nir) {
        //             (Some(tir), _) => {
        //                 log::debug!("{:#?}", tir);
        //                 tir
        //             }
        //             (_, errors) => {
        //                 println!("Error: {:#?}", errors);
        //                 src.clear();
        //                 continue;
        //             }
        //         };
        //         match eval(&src, env.clone(), tir) {
        //             Ok(val) => println!("{}", val),
        //             Err(err) => println!("Error: {:#?}", err),
        //         }
        //         src.clear();
        //     }
    }
}
