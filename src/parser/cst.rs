use crate::intern::InternedString;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseTree {
    Item(Item),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Data(Data),
    Decl(Decl),
    Expr(Expr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Data {
    pub name: InternedString,
    pub fields: Vec<InternedString>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Fn(FnDecl),
    Let(LetDecl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl {
    pub name: InternedString,
    pub params: Vec<InternedString>,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetDecl {
    pub name: InternedString,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    pub or: Or,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Or {
    pub and: And,
    pub or: Option<Box<Or>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct And {
    pub comp: Eq,
    pub and: Option<Box<And>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Eq {
    pub comp: Cmp,
    pub eq: Option<Box<Eq>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Cmp {
    pub add: Term,
    pub cmp: Option<(CmpOp, Box<Cmp>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CmpOp {
    Neq,
    Lt,
    Gt,
    Lte,
    Gte,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Term {
    pub factor: Factor,
    pub term: Option<(TermOp, Box<Term>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TermOp {
    Add,
    Sub,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Factor {
    pub unary: Power,
    pub factor: Option<(FactorOp, Box<Factor>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum FactorOp {
    Mul,
    Div,
    Mod,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Power {
    pub atom: Unary,
    pub power: Option<Box<Power>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Unary {
    Neg(Box<Unary>),
    Not(Box<Unary>),
    Apply(Apply),
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Apply {
    pub atom: Atom,
    pub arg: Atom,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {}
