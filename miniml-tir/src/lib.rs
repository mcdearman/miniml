use miniml_ast::{SynBoxNode, SynNode};
use miniml_nir::{scoped_ident::ScopedIdent, *};
use miniml_utils::{box_node::BoxNode, node::Node, span::Span};
use ty::Ty;

pub mod meta;
pub mod meta_context;
pub mod poly_type;
pub mod ty;

pub type TyNode<T> = Node<T, (Ty, Span)>;
pub type TyBoxNode<T> = BoxNode<T, (Ty, Span)>;

pub type Prog = SynNode<Module>;
pub type Imports = Vec<Path>;
pub type Path = Vec<SynNode<ScopedIdent>>;
pub type Decls = Vec<Decl>;
pub type Decl = TyNode<DeclKind>;
pub type Def = TyNode<DefKind>;
pub type Expr = TyBoxNode<ExprKind>;
pub type Pattern = TyBoxNode<PatternKind>;
pub type TypeAnno = SynBoxNode<TypeAnnoKind>;
pub type Ident = TyNode<ScopedIdent>;
