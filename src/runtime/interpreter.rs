use itertools::Itertools;

use super::{
    default_env,
    env::Env,
    error::{RuntimeError, RuntimeResult},
    eval::eval,
    value::Value,
};
use crate::{
    analysis::infer::TypeSolver,
    lex::token_iter::TokenIter,
    parse::parse,
    rename::resolver::Resolver,
    utils::{intern::InternedString, scoped_intern::ScopedInterner, unique_id::UniqueId},
};
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug)]
pub struct Interpreter {
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

        Self {
            res,
            builtins,
            scoped_interner,
            type_solver,
            env,
        }
    }

    pub fn run(&mut self, src: &str) -> RuntimeResult<Value> {
        let stream = TokenIter::new(&src);

        let ast = match parse(stream, true) {
            (Some(ast), _) => {
                log::debug!("AST: {:#?}", ast);
                // println!("AST: {:#?}", ast);
                ast
            }
            (None, parse_errors) => {
                return Err(RuntimeError::ParseError(
                    parse_errors
                        .iter()
                        .map(|e| InternedString::from(e.to_string()))
                        .collect_vec(),
                ));
            }
        };

        let nir = match self.res.resolve(&ast) {
            (Some(nir), _) => {
                log::debug!("NIR: {:#?}", nir);
                nir
            }
            (None, res_errors) => {
                return Err(RuntimeError::ResError(
                    res_errors
                        .iter()
                        .map(|e| InternedString::from(e.to_string()))
                        .collect_vec(),
                ));
            }
        };

        let tir = match self.type_solver.infer(&src, &nir) {
            (Some(tir), _) => {
                log::debug!("{:#?}", tir);
                tir
            }
            (None, errors) => {
                return Err(RuntimeError::InferenceError(
                    errors
                        .iter()
                        .map(|e| InternedString::from(e.to_string()))
                        .collect_vec(),
                ));
            }
        };

        eval(&src, self.env.clone(), tir)
    }
}
