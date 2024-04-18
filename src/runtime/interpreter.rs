use super::{default_env, env::Env};
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
    pub fn new() -> Self {
        let res = Resolver::new();
        let builtins = res.builtins().clone();
        let scoped_interner = res.env().dump_to_interner();
        let type_solver = TypeSolver::new(builtins.clone(), scoped_interner.clone());
        let env = default_env(builtins.clone());
        // let mut src = String::new();
        // let mut res = Resolver::new();
        // let builtins = res.builtins();
        // let scoped_interner = res.env().dump_to_interner();
        // let mut solver = TypeSolver::new(builtins.clone(), scoped_interner);
        // let env = default_env(builtins.clone());
        Self {
            src: String::new(),
            res,
            builtins,
            scoped_interner,
            type_solver,
            env,
        }
    }

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
