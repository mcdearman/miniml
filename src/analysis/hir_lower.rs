// use super::{
//     hir::{Env, Expr, Item, Lit, Root},
//     infer::{self, InfixOp, PrefixOp, Type},
// };
// use crate::util::{intern::InternedString, node::SrcNode, unique_id::UniqueId};
// use std::collections::HashMap;

// #[derive(Debug, Clone, PartialEq)]
// pub struct LowerError {
//     pub msg: InternedString,
// }

// impl LowerError {
//     pub fn new(msg: String) -> Self {
//         Self {
//             msg: InternedString::from(&*msg),
//         }
//     }
// }

// impl From<&str> for LowerError {
//     fn from(msg: &str) -> Self {
//         Self {
//             msg: InternedString::from(msg),
//         }
//     }
// }

// pub type LowerResult<T> = Result<T, LowerError>;

// #[derive(Debug, Clone)]
// pub struct OpIds {
//     prefix: HashMap<PrefixOp, UniqueId>,
//     infix: HashMap<InfixOp, UniqueId>,
// }

// impl OpIds {
//     pub fn new() -> Self {
//         Self {
//             prefix: HashMap::new(),
//             infix: HashMap::new(),
//         }
//     }

//     pub fn get_prefix(&self, op: &PrefixOp) -> UniqueId {
//         *self.prefix.get(op).unwrap()
//     }

//     pub fn get_infix(&self, op: &InfixOp) -> UniqueId {
//         *self.infix.get(op).unwrap()
//     }

//     pub fn insert_prefix(&mut self, op: PrefixOp, id: UniqueId) {
//         self.prefix.insert(op, id);
//     }

//     pub fn insert_infix(&mut self, op: InfixOp, id: UniqueId) {
//         self.infix.insert(op, id);
//     }
// }

// pub fn lower(root: SrcNode<infer::Root>) -> LowerResult<SrcNode<Root>> {
//     let mut env = Env::new();
//     let mut items = Vec::new();

//     let mut op_ids = OpIds {
//         prefix: HashMap::new(),
//         infix: HashMap::new(),
//     };
//     op_ids.insert_prefix(PrefixOp::Neg, UniqueId::gen());
//     op_ids.insert_prefix(PrefixOp::Not, UniqueId::gen());
//     op_ids.insert_infix(InfixOp::Add, UniqueId::gen());
//     op_ids.insert_infix(InfixOp::Sub, UniqueId::gen());
//     op_ids.insert_infix(InfixOp::Mul, UniqueId::gen());
//     op_ids.insert_infix(InfixOp::Div, UniqueId::gen());

//     for item in root.inner().clone().items {
//         let item = lower_item(item.clone(), &mut env, &mut op_ids)?;
//         items.push(item);
//     }
//     Ok(SrcNode::new(Root { items }, root.span()))
// }

// fn lower_item(
//     item: SrcNode<infer::Item>,
//     env: &mut Env,
//     op_ids: &mut OpIds,
// ) -> LowerResult<SrcNode<Item>> {
//     match item.inner() {
//         infer::Item::Expr(expr) => Ok(SrcNode::new(
//             Item::Expr(
//                 lower_expr(SrcNode::new(expr.clone(), item.span()), env, op_ids)?
//                     .inner()
//                     .clone(),
//             ),
//             item.span(),
//         )),
//         infer::Item::Def { name, expr } => {
//             let expr = lower_expr(expr.clone(), env, op_ids)?;
//             env.insert(name.inner().clone(), expr.inner().clone());
//             Ok(SrcNode::new(
//                 Item::Def {
//                     name: name.clone(),
//                     body: expr,
//                 },
//                 item.span(),
//             ))
//         }
//     }
// }

// fn lower_expr(
//     expr: SrcNode<infer::Expr>,
//     env: &mut Env,
//     op_ids: &mut OpIds,
// ) -> LowerResult<SrcNode<Expr>> {
//     match expr.inner().clone() {
//         infer::Expr::Lit { lit, ty } => match lit {
//             infer::Lit::Num(n) => Ok(SrcNode::new(
//                 Expr::Lit {
//                     lit: Lit::Num(n),
//                     ty: ty.clone(),
//                 },
//                 expr.span(),
//             )),
//             infer::Lit::Bool(b) => Ok(SrcNode::new(
//                 Expr::Lit {
//                     lit: Lit::Bool(b),
//                     ty: ty.clone(),
//                 },
//                 expr.span(),
//             )),
//         },
//         infer::Expr::Ident { name, ty } => Ok(SrcNode::new(
//             Expr::Ident {
//                 name,
//                 ty: ty.clone(),
//             },
//             expr.span(),
//         )),
//         infer::Expr::Lambda { params, body, ty } => {
//             let mut env = Env::new_with_parent(env.clone());
//             let body = lower_expr(*body.clone(), &mut env, op_ids)?;
//             Ok(SrcNode::new(
//                 Expr::Lambda {
//                     env: Box::new(env),
//                     params: params.clone(),
//                     body: Box::new(body),
//                     ty: ty.clone(),
//                 },
//                 expr.span(),
//             ))
//         }
//         infer::Expr::Apply { fun, args, ty } => {
//             let fun = lower_expr(*fun.clone(), env, op_ids)?;
//             let args = args
//                 .iter()
//                 .map(|arg| lower_expr(arg.clone(), env, op_ids))
//                 .collect::<LowerResult<Vec<_>>>()?;
//             Ok(SrcNode::new(
//                 Expr::Apply {
//                     fun: Box::new(fun),
//                     args,
//                     ty: ty.clone(),
//                 },
//                 expr.span(),
//             ))
//         }
//         infer::Expr::Let {
//             name,
//             expr,
//             body,
//             ty,
//         } => {
//             // lambda lifting for let
//             todo!()
//             // let mut env = Env::new_with_parent(env.clone());
//             // let expr = lower_expr(*expr.clone(), &mut env)?;
//             // env.insert(name.inner().clone(), expr.inner().clone());
//             // let body = lower_expr(*body.clone(), &mut env)?;
//             // Ok(SrcNode::new(
//             //     Expr::Apply {
//             //         fun: Box::new(Expr::Lambda { env: Env::new(), params: , body: (), ty: () }),
//             //         args: vec![SrcNode::new(
//             //             Expr::Ident {
//             //                 expr: name.clone(),
//             //                 ty: ty.clone(),
//             //             },
//             //             name.span(),
//             //         )],
//             //         ty: ty.clone(),
//             //     },
//             //     expr.span(),
//             // ))
//         }
//         infer::Expr::Infix { op, lhs, rhs, ty } => {
//             // convert op exprs to applications
//             let l = lower_expr(*lhs.clone(), env, op_ids)?;
//             let r = lower_expr(*rhs.clone(), env, op_ids)?;
//             Ok(SrcNode::new(
//                 Expr::Apply {
//                     fun: Box::new(SrcNode::new(
//                         Expr::Ident {
//                             name: SrcNode::new(op_ids.get_infix(op.inner()), op.span()),
//                             ty: Type::Lambda(vec![lhs.ty(), rhs.ty()], Box::new(ty.clone())),
//                         },
//                         op.span(),
//                     )),
//                     args: vec![l, r],
//                     ty,
//                 },
//                 expr.span(),
//             ))
//         }
//         infer::Expr::Prefix { op, expr: rhs, ty } => {
//             // convert op exprs to applications
//             let e = lower_expr(*rhs.clone(), env, op_ids)?;
//             Ok(SrcNode::new(
//                 Expr::Apply {
//                     fun: Box::new(SrcNode::new(
//                         Expr::Ident {
//                             name: SrcNode::new(op_ids.get_prefix(op.inner()), op.span()),
//                             ty: Type::Lambda(vec![rhs.ty()], Box::new(ty.clone())),
//                         },
//                         op.span(),
//                     )),
//                     args: vec![e.clone()],
//                     ty,
//                 },
//                 expr.span(),
//             ))
//         }
//         infer::Expr::Unit => Ok(SrcNode::new(Expr::Unit, expr.span())),
//     }
// }
