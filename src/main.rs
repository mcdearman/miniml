use analysis::{
    hir_lower,
    infer::{type_inference, Context},
    res::resolve,
};
use syntax::parse::parse;

mod analysis;
mod runtime;
mod syntax;
mod util;

fn main() {
    // f: 'a -> 'b
    // x: 'a
    // f x: 'b = 'a -> 'b
    // f x = f
    let (ast, errors) = parse("add x = \\y -> x + y");
    if !errors.is_empty() {
        panic!("{:?}", errors);
    }
    let (res, errors) = resolve(&ast.unwrap());
    if !errors.is_empty() {
        panic!("{:?}", errors);
    }
    let ctx = Context::new();
    let infer = match type_inference(ctx, res.unwrap()) {
        Ok(root) => root,
        Err(errors) => panic!("inference failed: {:?}", errors),
    };

    println!("infer: {:?}", infer);

    match hir_lower::lower(infer) {
        Ok(hir) => println!("hir: {:?}", hir),
        Err(errors) => panic!("lowering failed: {:?}", errors),
    }
}
