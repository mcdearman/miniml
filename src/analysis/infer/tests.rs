use crate::{
    analysis::infer::TypeSolver,
    lex::{token::Token, token_iter::TokenIter},
    parse::parse,
    rename::resolver::Resolver,
};
use logos::Logos;

fn test_helper(src: &str) {
    let lexer = TokenIter::new(src);
    let (ast, errors) = parse(lexer, true);
    if !errors.is_empty() {
        for e in errors {
            eprintln!("{}", e);
        }
        panic!("parsing failed");
    }
    let mut res = Resolver::new();
    let (nir, errors) = res.resolve(&ast.unwrap());
    if !errors.is_empty() {
        for e in errors {
            eprintln!("{}", e);
        }
        panic!("resolution failed");
    }
    let nir = nir.unwrap();
    let builtins = res.builtins();
    let scoped_interner = res.env().dump_to_interner();
    if !errors.is_empty() {
        for e in errors {
            eprintln!("{}", e);
        }
        panic!("resolution failed");
    }
    let mut solver = TypeSolver::new(builtins.clone(), scoped_interner);
    let (tir, errors) = solver.infer(src, &nir);
    if !errors.is_empty() {
        for e in errors {
            eprintln!("{}", e);
        }
        panic!("type inference failed");
    }
    let tir = tir.unwrap();
    insta::assert_debug_snapshot!(tir);
}

#[test]
fn test_infer_int() {
    test_helper("42");
}

#[test]
fn test_infer_let_decl() {
    test_helper("let x = 42");
}
