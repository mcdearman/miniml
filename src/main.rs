use runtime::repl::repl;

mod analysis;
mod runtime;
mod syntax;
mod util;

fn main() {
    env_logger::init();
    repl();
    // f: 'a -> 'b
    // x: 'a
    // f x: 'b = 'a -> 'b
    // f x = f
    // let src = "x = 1";
    // let (ast, errors) = parse(src);
    // if !errors.is_empty() {
    //     panic!("{:?}", errors);
    // }
    // let (res, errors) = resolve(&ast.unwrap());
    // if !errors.is_empty() {
    //     panic!("{:?}", errors);
    // }
    // let ctx = Context::new();
    // let infer = match type_inference(ctx, res.unwrap()) {
    //     Ok(root) => root,
    //     Err(errors) => panic!("inference failed: {:?}", errors),
    // };

    // println!("infer: {:?}", infer);

    // // match hir_lower::lower(infer) {
    // //     Ok(hir) => println!("hir: {:?}", hir),
    // //     Err(errors) => panic!("lowering failed: {:?}", errors),
    // // }

    // match runtime::tree_walk::eval(src, &infer) {
    //     Ok(val) => println!("eval: {}", val),
    //     Err(errors) => panic!("evaluation failed: {:?}", errors),
    // }
}
