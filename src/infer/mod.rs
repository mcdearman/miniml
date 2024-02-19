use itertools::Itertools;

use self::{
    constraint::Constraint,
    context::Context,
    error::{InferResult, TypeError},
    r#type::Type,
    scheme::Scheme,
    substitution::Substitution,
    tir::*,
    ty_var::TyVar,
};
use crate::{
    rename::nir,
    utils::{intern::InternedString, unique_id::UniqueId},
};
use std::collections::HashMap;

mod constraint;
pub mod context;
pub mod error;
mod scheme;
pub mod substitution;
pub mod tir;
mod ty_var;
pub mod r#type;

pub fn infer<'src>(
    src: &'src str,
    ctx: &mut Context,
    builtins: HashMap<UniqueId, InternedString>,
    nir: &nir::Root,
) -> InferResult<(Root, Context)> {
    let mut s = Substitution::new();
    let mut decls = vec![];
    let mut ctx_ret = Context::new();
    for decl in nir.decls() {
        let (cs, ctx, d) = infer_decl(src, ctx, builtins.clone(), decl)?;
        let mut traits = vec![];
        for c in cs {
            // println!("constraint: {:?}", c);
            match c {
                Constraint::Equal(t1, t2) => {
                    let new_sub = unify(t1.apply_subst(s.clone()), t2.apply_subst(s.clone()))?;
                    // println!("new_sub: {:?}", new_sub);
                    s = new_sub.compose(s);
                    // println!("s = new_sub.compose(s): {:?}", s);
                }
                _ => {
                    traits.push(c);
                }
            }
        }
        for t in traits {
            match t {
                Constraint::Neg(t1, t2) => {
                    match (t1.apply_subst(s.clone()), t2.apply_subst(s.clone())) {
                        (Type::Int, Type::Int) => {}
                        _ => {
                            return Err(TypeError::from(format!(
                                "expected Neg : (Int -> Int), found ({:?} -> {:?})",
                                t1.lower(&mut HashMap::new()),
                                t2.lower(&mut HashMap::new())
                            )))
                        }
                    }
                }
                Constraint::Not(t1, t2) => {
                    match (t1.apply_subst(s.clone()), t2.apply_subst(s.clone())) {
                        (Type::Bool, Type::Bool) => {}
                        _ => {
                            return Err(TypeError::from(format!(
                                "expected Not : (Bool -> Bool), found ({:?} -> {:?})",
                                t1.lower(&mut HashMap::new()),
                                t2.lower(&mut HashMap::new())
                            )))
                        }
                    }
                }
                Constraint::Add(t1, t2, t3) => match (
                    t1.apply_subst(s.clone()),
                    t2.apply_subst(s.clone()),
                    t3.apply_subst(s.clone()),
                ) {
                    (Type::Int, Type::Int, Type::Int) => {}
                    _ => {
                        return Err(TypeError::from(format!(
                            "expected Add : (Int Int -> Int), found ({:?} {:?} -> {:?})",
                            t1.lower(&mut HashMap::new()),
                            t2.lower(&mut HashMap::new()),
                            t3.lower(&mut HashMap::new())
                        )))
                    }
                },
                Constraint::Sub(t1, t2, t3) => match (
                    t1.apply_subst(s.clone()),
                    t2.apply_subst(s.clone()),
                    t3.apply_subst(s.clone()),
                ) {
                    (Type::Int, Type::Int, Type::Int) => {}
                    _ => {
                        return Err(TypeError::from(format!(
                            "expected Sub : (Int Int -> Int), found ({:?} {:?} -> {:?})",
                            t1.lower(&mut HashMap::new()),
                            t2.lower(&mut HashMap::new()),
                            t3.lower(&mut HashMap::new())
                        )))
                    }
                },
                Constraint::Mul(t1, t2, t3) => match (
                    t1.apply_subst(s.clone()),
                    t2.apply_subst(s.clone()),
                    t3.apply_subst(s.clone()),
                ) {
                    (Type::Int, Type::Int, Type::Int) => {}
                    _ => {
                        return Err(TypeError::from(format!(
                            "expected Mul : (Int Int -> Int), found ({:?} {:?} -> {:?})",
                            t1.lower(&mut HashMap::new()),
                            t2.lower(&mut HashMap::new()),
                            t3.lower(&mut HashMap::new())
                        )))
                    }
                },
                Constraint::Div(t1, t2, t3) => match (
                    t1.apply_subst(s.clone()),
                    t2.apply_subst(s.clone()),
                    t3.apply_subst(s.clone()),
                ) {
                    (Type::Int, Type::Int, Type::Int) => {}
                    _ => {
                        return Err(TypeError::from(format!(
                            "expected Div : (Int Int -> Int), found ({:?} {:?} -> {:?})",
                            t1.lower(&mut HashMap::new()),
                            t2.lower(&mut HashMap::new()),
                            t3.lower(&mut HashMap::new())
                        )))
                    }
                },
                Constraint::Rem(t1, t2, t3) => {
                    let lhs = t1.apply_subst(s.clone());
                    let rhs = t2.apply_subst(s.clone());
                    let ret = t3.apply_subst(s.clone());
                    match (&lhs, &rhs) {
                        (Type::Int, Type::Int) => {
                            s = unify(ret, Type::Int)?.compose(s);
                        }
                        _ => {
                            let m = &mut HashMap::new();
                            return Err(TypeError::from(format!(
                                "expected Rem : (Int Int -> Int), found ({:?} {:?} -> {:?})",
                                lhs.lower(m),
                                rhs.lower(m),
                                ret.lower(m)
                            )));
                        }
                    }
                }
                Constraint::Pow(t1, t2, t3) => match (
                    t1.apply_subst(s.clone()),
                    t2.apply_subst(s.clone()),
                    t3.apply_subst(s.clone()),
                ) {
                    (Type::Int, Type::Int, Type::Int) => {}
                    _ => {
                        return Err(TypeError::from(format!(
                            "expected Pow : (Int Int -> Int), found ({:?} {:?} -> {:?})",
                            t1.lower(&mut HashMap::new()),
                            t2.lower(&mut HashMap::new()),
                            t3.lower(&mut HashMap::new())
                        )))
                    }
                },
                _ => {}
            }
        }
        ctx_ret = ctx_ret.union(ctx);
        decls.push(d);
    }
    Ok((
        Root::new(decls, *nir.span()).apply_subst(s.clone()),
        ctx_ret.apply_subst(s.clone()).apply_subst(s),
    ))
}

fn infer_decl<'src>(
    src: &'src str,
    ctx: &mut Context,
    builtins: HashMap<UniqueId, InternedString>,
    decl: &nir::Decl,
) -> InferResult<(Vec<Constraint>, Context, Decl)> {
    match decl.kind() {
        nir::DeclKind::Let { name, expr } => {
            let (cs, ty, ectx, expr) = match expr.kind() {
                nir::ExprKind::Lambda { .. } => {
                    let ty = Type::Var(TyVar::fresh());
                    let scheme = Scheme::generalize(ctx.clone(), ty.clone());
                    ctx.extend(*name.id(), scheme.clone());
                    infer_expr(src, ctx, builtins, expr)?
                }
                _ => {
                    let (cs, ty, ectx, expr) = infer_expr(src, ctx, builtins, expr)?;
                    let scheme = Scheme::generalize(ectx.clone(), ty.clone());
                    let mut tmp_ctx = ectx.clone();
                    tmp_ctx.extend(*name.id(), scheme.clone());
                    (cs, ty, tmp_ctx, expr)
                }
            };
            Ok((
                cs,
                ctx.union(ectx),
                Decl::new(
                    DeclKind::Let {
                        name: Ident::new(*name.id(), *name.span()),
                        expr,
                    },
                    ty,
                    *decl.span(),
                ),
            ))
        }
    }
}

fn infer_expr<'src>(
    src: &'src str,
    ctx: &mut Context,
    builtins: HashMap<UniqueId, InternedString>,
    expr: &nir::Expr,
) -> InferResult<(Vec<Constraint>, Type, Context, Expr)> {
    match expr.kind() {
        nir::ExprKind::Lit(lit) => match *lit {
            nir::Lit::Int(n) => Ok((
                vec![],
                Type::Int,
                ctx.clone(),
                Expr::new(ExprKind::Lit(Lit::Int(n)), Type::Int, *expr.span()),
            )),
            nir::Lit::Bool(b) => Ok((
                vec![],
                Type::Bool,
                ctx.clone(),
                Expr::new(ExprKind::Lit(Lit::Bool(b)), Type::Bool, *expr.span()),
            )),
        },
        nir::ExprKind::Ident(name) => {
            if let Some(scm) = ctx.get(name.id()) {
                let ty = scm.instantiate();
                // println!("inst ident: {:?} {:?}", name, ty);
                Ok((
                    vec![],
                    ty.clone(),
                    ctx.clone(),
                    Expr::new(
                        ExprKind::Ident(Ident::new(*name.id(), *name.span())),
                        ty.clone(),
                        *expr.span(),
                    ),
                ))
            } else {
                Err(TypeError::from(format!(
                    "unbound variable: {:?} - \"{}\"",
                    expr,
                    src[*name.span()].to_string()
                )))
            }
        }
        nir::ExprKind::Lambda { params, expr } => {
            let mut ty_binders = vec![];
            let mut tmp_ctx = ctx.clone();
            let mut new_params = vec![];
            for p in params.clone() {
                let ty_binder = Type::Var(TyVar::fresh());
                ty_binders.push(ty_binder.clone());
                tmp_ctx = ctx.union(tmp_ctx);
                tmp_ctx.extend(*p.id(), Scheme::new(vec![], ty_binder.clone()));
                new_params.push(Ident::new(*p.id(), *p.span()));
            }
            let (cs, t, c, expr) = infer_expr(src, &mut tmp_ctx, builtins, expr)?;
            let ty = Type::Lambda(ty_binders.clone(), Box::new(t.clone()));
            Ok((
                cs,
                ty.clone(),
                tmp_ctx.union(c),
                Expr::new(
                    ExprKind::Lambda {
                        params: new_params,
                        expr: expr.clone(),
                    },
                    ty,
                    *expr.span(),
                ),
            ))
        }
        nir::ExprKind::Apply { fun, args } => {
            let (mut cs1, t1, mut ctx1, fun) = infer_expr(src, ctx, builtins.clone(), fun)?;
            let mut new_args = vec![];
            let mut ty_args = vec![];
            for a in args {
                let (cs2, t2, ctx2, e2) = infer_expr(src, &mut ctx1, builtins.clone(), a)?;
                ctx1 = ctx2.union(ctx1);
                cs1 = cs1.into_iter().chain(cs2.into_iter()).collect();
                new_args.push(e2);
                ty_args.push(t2);
            }
            let ty_ret = Type::Var(TyVar::fresh());
            match fun.kind() {
                ExprKind::Ident(name) => match builtins.get(name.id()) {
                    Some(builtin) => match builtin.as_ref() {
                        "neg" => cs1.push(Constraint::Neg(ty_args[0].clone(), ty_ret.clone())),
                        "not" => cs1.push(Constraint::Not(ty_args[0].clone(), ty_ret.clone())),
                        "add" => cs1.push(Constraint::Add(
                            ty_args[0].clone(),
                            ty_args[1].clone(),
                            ty_ret.clone(),
                        )),
                        "sub" => cs1.push(Constraint::Sub(
                            ty_args[0].clone(),
                            ty_args[1].clone(),
                            ty_ret.clone(),
                        )),
                        "mul" => cs1.push(Constraint::Mul(
                            ty_args[0].clone(),
                            ty_args[1].clone(),
                            ty_ret.clone(),
                        )),
                        "div" => cs1.push(Constraint::Div(
                            ty_args[0].clone(),
                            ty_args[1].clone(),
                            ty_ret.clone(),
                        )),
                        "rem" => cs1.push(Constraint::Rem(
                            ty_args[0].clone(),
                            ty_args[1].clone(),
                            ty_ret.clone(),
                        )),
                        "pow" => cs1.push(Constraint::Pow(
                            ty_args[0].clone(),
                            ty_args[1].clone(),
                            ty_ret.clone(),
                        )),
                        _ => {}
                    },
                    _ => {}
                },
                _ => {}
            }
            cs1.push(Constraint::Equal(
                t1.clone(),
                Type::Lambda(ty_args, Box::new(ty_ret.clone())),
            ));
            // println!("apply constraints: {:#?}", cs1);
            Ok((
                cs1,
                ty_ret.clone(),
                ctx1,
                Expr::new(
                    ExprKind::Apply {
                        fun,
                        args: new_args,
                    },
                    ty_ret,
                    *expr.span(),
                ),
            ))
        }
        nir::ExprKind::Let { name, expr, body } => {
            let (cs, ty, mut ctx, expr) = match expr.kind() {
                nir::ExprKind::Lambda { expr, .. } => {
                    let ty = Type::Var(TyVar::fresh());
                    let scheme = Scheme::generalize(ctx.clone(), ty.clone());
                    ctx.extend(*name.id(), scheme);
                    infer_expr(src, ctx, builtins.clone(), expr)?
                }
                _ => {
                    let (cs, ty, ectx, expr) = infer_expr(src, ctx, builtins.clone(), expr)?;
                    let scheme = Scheme::generalize(ectx.clone(), ty.clone());
                    let mut tmp_ctx = ectx.clone();
                    tmp_ctx.extend(*name.id(), scheme);
                    (cs, ty, tmp_ctx, expr)
                }
            };
            let (cs_body, ty_body, ctx_body, body) = infer_expr(src, &mut ctx, builtins, body)?;
            ctx = ctx_body.union(ctx);
            let cs = cs
                .into_iter()
                .chain(cs_body.into_iter())
                .collect::<Vec<_>>();
            Ok((
                cs,
                ty_body.clone(),
                ctx,
                Expr::new(
                    ExprKind::Let {
                        name: Ident::new(*name.id(), *name.span()),
                        expr: expr.clone(),
                        body,
                    },
                    ty_body,
                    *expr.span(),
                ),
            ))
        }
        nir::ExprKind::If { cond, then, else_ } => {
            let (cs1, t1, mut ctx1, cond) = infer_expr(src, ctx, builtins.clone(), cond)?;
            let (cs2, t2, mut ctx2, then) = infer_expr(src, &mut ctx1, builtins.clone(), then)?;
            let (cs3, t3, ctx3, else_) = infer_expr(src, &mut ctx2, builtins, else_)?;
            let ctx = ctx3.union(ctx2).union(ctx1);
            let mut cs = cs1
                .into_iter()
                .chain(cs2.into_iter())
                .chain(cs3.into_iter())
                .collect_vec();
            cs.push(Constraint::Equal(t1, Type::Bool));
            cs.push(Constraint::Equal(t2.clone(), t3));
            // println!("if constraints: {:#?}", cs);
            Ok((
                cs,
                t2.clone(),
                ctx,
                Expr::new(ExprKind::If { cond, then, else_ }, t2, *expr.span()),
            ))
        }
        nir::ExprKind::Unit => Ok((
            vec![],
            Type::Unit,
            ctx.clone(),
            Expr::new(ExprKind::Unit, Type::Unit, *expr.span()),
        )),
    }
}

fn unify(t1: Type, t2: Type) -> InferResult<Substitution> {
    // println!("unify: {:?} and {:?}", t1, t2);
    match (t1.clone(), t2.clone()) {
        (Type::Int, Type::Int) | (Type::Bool, Type::Bool) | (Type::Unit, Type::Unit) => {
            Ok(Substitution::new())
        }
        (Type::Lambda(p1, b1), Type::Lambda(p2, b2)) => {
            let s1 = p1.into_iter().zip(p2.into_iter()).fold(
                Ok(Substitution::new()),
                |acc: InferResult<Substitution>, (t1, t2)| {
                    let s = acc?;
                    let t1 = t1.apply_subst(s.clone());
                    let t2 = t2.apply_subst(s.clone());
                    // println!("unify params: {:?} {:?}", t1, t2);
                    let s1 = unify(t1, t2)?;
                    // println!("unify args: {:?} {:?}", s, s1);
                    Ok(s1.compose(s))
                },
            )?;
            // println!("unify s1: {:?}", s1);
            let s2 = unify(b1.apply_subst(s1.clone()), b2.apply_subst(s1.clone()))?;
            // println!("unify s2: {:?}", s2);
            // println!("unify s1.compose(s2): {:?}", s1.compose(s2.clone()));
            Ok(s1.compose(s2.clone()))
        }
        (_, Type::Var(n)) => var_bind(n, t1),
        (Type::Var(n), _) => var_bind(n, t2),
        _ => Err(TypeError::from(format!(
            "cannot unify {:?} and {:?}",
            t1.lower(&mut HashMap::new()),
            t2.lower(&mut HashMap::new())
        ))),
    }
}

fn var_bind(var: TyVar, ty: Type) -> InferResult<Substitution> {
    if ty.clone() == Type::Var(var.clone()) {
        Ok(Substitution::new())
    } else if ty.free_vars().contains(&var) {
        Err(TypeError::from(format!(
            "occurs check failed: {} occurs in {:?}",
            var, ty
        )))
    } else {
        let mut subst = Substitution::new();
        subst.insert(var, ty.clone());
        Ok(subst)
    }
}

mod tests {
    // use super::{res::resolve, Context, Root};
    // use crate::res;
    // use common::node::Node;
    // use syntax::chumsky_parser::parse;

    // fn test_helper(src: &str) -> (Node<Root>, Context) {
    //     let (ast, errors) = parse(src);
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }

    //     let env = res::Env::new();
    //     let (res, errors) = resolve(env, &ast.unwrap());
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }

    //     let mut ctx = super::Context::new();
    //     super::type_inference(src, &mut ctx, res.unwrap()).unwrap()
    // }

    // #[test]
    // fn infer_num() {
    //     insta::assert_debug_snapshot!(test_helper("1"));
    // }

    // #[test]
    // fn infer_bool() {
    //     insta::assert_debug_snapshot!(test_helper("true"));
    // }

    // #[test]
    // fn infer_def() {
    //     insta::assert_debug_snapshot!(test_helper("x = 1"));
    // }

    // #[test]
    // fn infer_prefix() {
    //     insta::assert_debug_snapshot!(test_helper("let x = 1 in -x"));
    // }

    // #[test]
    // fn infer_infix() {
    //     let (ast, errors) = parse("1 + 2");
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }
    //     let (res, errors) = resolve(&ast.unwrap());
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }

    //     let ctx = super::Context::new();
    //     if let Ok(types) = super::type_inference(ctx, res.unwrap()) {
    //         insta::assert_debug_snapshot!(types);
    //     } else {
    //         panic!("inference failed: {:?}", errors);
    //     }
    // }

    // #[test]
    // fn infer_fn_def() {
    //     let (ast, errors) = parse("f x = x");
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }
    //     let (res, errors) = resolve(&ast.unwrap());
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }

    //     let ctx = super::Context::new();
    //     if let Ok(types) = super::type_inference(ctx, res.unwrap()) {
    //         insta::assert_debug_snapshot!(types);
    //     } else {
    //         panic!("inference failed: {:?}", errors);
    //     }
    // }

    // #[test]
    // fn infer_infix_fn_def() {
    //     let (ast, errors) = parse("add x y = x + y");
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }
    //     let (res, errors) = resolve(&ast.unwrap());
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }

    //     let ctx = super::Context::new();
    //     if let Ok(types) = super::type_inference(ctx, res.unwrap()) {
    //         insta::assert_debug_snapshot!(types);
    //     } else {
    //         panic!("inference failed: {:?}", errors);
    //     }
    // }

    // #[test]
    // fn infer_apply_second_order() {
    //     let (ast, errors) = parse("f g x = g x");
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }
    //     let (res, errors) = resolve(&ast.unwrap());
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }

    //     let ctx = super::Context::new();
    //     if let Ok(types) = super::type_inference(ctx, res.unwrap()) {
    //         insta::assert_debug_snapshot!(types);
    //     } else {
    //         panic!("inference failed: {:?}", errors);
    //     }
    // }

    // #[test]
    // fn infer_apply_rec() {
    //     let (ast, errors) = parse("f x = f x");
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }
    //     let (res, errors) = resolve(&ast.unwrap());
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }

    //     let ctx = super::Context::new();
    //     match super::type_inference(ctx, res.unwrap()) {
    //         Ok(types) => insta::assert_debug_snapshot!(types),
    //         Err(errors) => panic!("inference failed: {:?}", errors),
    //     }
    // }

    // #[test]
    // fn infer_let() {
    //     let (ast, errors) = parse("let x = 1 in x");
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }
    //     let (res, errors) = resolve(&ast.unwrap());
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }
    //     let ctx = super::Context::new();
    //     match super::type_inference(ctx, res.unwrap()) {
    //         Ok(types) => insta::assert_debug_snapshot!(types),
    //         Err(errors) => panic!("inference failed: {:?}", errors),
    //     }
    // }

    // #[test]
    // fn infer_let_infix() {
    //     let (ast, errors) = parse("let x = 1 in x + 1");
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }
    //     let (res, errors) = resolve(&ast.unwrap());
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }
    //     let ctx = super::Context::new();
    //     match super::type_inference(ctx, res.unwrap()) {
    //         Ok(types) => insta::assert_debug_snapshot!(types),
    //         Err(errors) => panic!("inference failed: {:?}", errors),
    //     }
    // }

    // #[test]
    // fn infer_let_fn() {
    //     let (ast, errors) = parse("let id x = x in id id");
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }
    //     let (res, errors) = resolve(&ast.unwrap());
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }
    //     let ctx = super::Context::new();
    //     match super::type_inference(ctx, res.unwrap()) {
    //         Ok(types) => insta::assert_debug_snapshot!(types),
    //         Err(errors) => panic!("inference failed: {:?}", errors),
    //     }
    // }

    // #[test]
    // fn infer_let_rec() {
    //     let (ast, errors) = parse("let f x = f x in f f");
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }
    //     let (res, errors) = resolve(&ast.unwrap());
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }
    //     let ctx = super::Context::new();
    //     match super::type_inference(ctx, res.unwrap()) {
    //         Ok(types) => insta::assert_debug_snapshot!(types),
    //         Err(errors) => panic!("inference failed: {:?}", errors),
    //     }
    // }

    // #[test]
    // fn infer_rec_loop() {
    //     let (ast, errors) = parse("f x = f");
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }
    //     let (res, errors) = resolve(&ast.unwrap());
    //     if !errors.is_empty() {
    //         panic!("{:?}", errors);
    //     }
    //     let ctx = super::Context::new();
    //     match super::type_inference(ctx, res.unwrap()) {
    //         Ok(types) => insta::assert_debug_snapshot!(types),
    //         Err(errors) => panic!("inference failed: {:?}", errors),
    //     }
    // }

    // #[test]
    // fn infer_infinite() {
    //     let res = res::Root {
    //         items: vec![Node::new(res::Item::Def {
    //             name: Node::new(0.into(), 0..0),
    //             expr: Node::new(
    //                 res::Expr::Apply {
    //                     fun: Box::new(Node::new(res::Expr::Ident(0.into()), 0..0)),
    //                     args: vec![Node::new(res::Expr::Ident(0.into()), 0..0)],
    //                 },
    //                 0..0,
    //             ),
    //         })],
    //     };
    // }
}
