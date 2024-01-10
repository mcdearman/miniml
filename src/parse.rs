use crate::utils::{InternedString, Span};
use chumsky::{
    error::Rich,
    extra,
    input::{Input, Stream, ValueInput},
    primitive::just,
    recursive::recursive,
    select, IterParser, Parser as ChumskyParser,
};
use logos::Logos;
use num_rational::Rational64;
use std::fmt::Display;

#[derive(Logos, Debug, Clone, PartialEq)]
pub enum Token {
    Error,
    #[regex(r"--.*", logos::skip)]
    Comment,
    #[regex(r"[ \t\n\r]+", logos::skip)]
    Whitespace,

    // Literals and identifiers
    #[regex(
        r"-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0))(/-?((0b[0-1]+)|(0o[0-7]+)|(0x[0-9a-fA-F]+)|([1-9]\d*|0)))?", 
        |lex| lex.slice().parse().ok())]
    Num(Rational64),
    #[regex(r"true|false", |lex| lex.slice().parse().ok())]
    Bool(bool),
    #[regex(r#""(\\.|[^"\\])*""#, |lex| InternedString::from(lex.slice()))]
    String(InternedString),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| InternedString::from(lex.slice()))]
    Ident(InternedString),

    // Punctuation
    #[token("\\")]
    Backslash,
    #[token("->")]
    RArrow,
    #[token("=")]
    Assign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,
    #[token("^")]
    Caret,
    #[token("||")]
    Or,
    #[token("&&")]
    And,
    #[token("==")]
    Eq,
    #[token("!=")]
    Neq,
    #[token("<")]
    Lt,
    #[token(">")]
    Gt,
    #[token("<=")]
    Leq,
    #[token(">=")]
    Geq,
    #[token("!")]
    Bang,
    #[token(",")]
    Comma,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBrack,
    #[token("]")]
    RBrack,

    // Keywords
    #[token("let")]
    Let,
    #[token("in")]
    In,
    #[token("if")]
    If,
    #[token("then")]
    Then,
    #[token("else")]
    Else,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use Token::*;
        match self {
            Error => write!(f, "Error"),
            Comment => write!(f, "Comment"),
            Whitespace => write!(f, "Whitespace"),
            Num(n) => write!(f, "Num({})", n),
            Bool(b) => write!(f, "Bool({})", b),
            String(s) => write!(f, "String({})", s),
            Ident(s) => write!(f, "Ident({})", s),
            Backslash => write!(f, "Backslash"),
            RArrow => write!(f, "RArrow"),
            Assign => write!(f, "Assign"),
            Plus => write!(f, "Plus"),
            Minus => write!(f, "Minus"),
            Star => write!(f, "Star"),
            Slash => write!(f, "Slash"),
            Percent => write!(f, "Percent"),
            Caret => write!(f, "Caret"),
            Or => write!(f, "Or"),
            And => write!(f, "And"),
            Eq => write!(f, "Eq"),
            Neq => write!(f, "Neq"),
            Lt => write!(f, "Lt"),
            Gt => write!(f, "Gt"),
            Leq => write!(f, "Leq"),
            Geq => write!(f, "Geq"),
            Bang => write!(f, "Bang"),
            Comma => write!(f, "Comma"),
            LParen => write!(f, "LParen"),
            RParen => write!(f, "RParen"),
            LBrace => write!(f, "LBrace"),
            RBrace => write!(f, "RBrace"),
            LBrack => write!(f, "LBrack"),
            RBrack => write!(f, "RBrack"),
            Let => write!(f, "Let"),
            In => write!(f, "In"),
            If => write!(f, "If"),
            Then => write!(f, "Then"),
            Else => write!(f, "Else"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Root {
    decls: Vec<Decl>,
    span: Span,
}

impl Root {
    pub fn new(decls: Vec<Decl>, span: Span) -> Self {
        Self { decls, span }
    }

    pub fn decls(&self) -> &[Decl] {
        &self.decls
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Item {
    kind: ItemKind,
    span: Span,
}

impl Item {
    pub fn new(kind: ItemKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &ItemKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ItemKind {
    Expr(Expr),
    Decl(Decl),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expr {
    kind: Box<ExprKind>,
    span: Span,
}

impl Expr {
    pub fn new(kind: ExprKind, span: Span) -> Self {
        Self {
            kind: Box::new(kind),
            span,
        }
    }

    pub fn kind(&self) -> &ExprKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprKind {
    Lit(Lit),
    Ident(Ident),
    Apply { fun: Expr, arg: Expr },
    Unary { op: UnaryOp, expr: Expr },
    Binary { op: BinaryOp, lhs: Expr, rhs: Expr },
    If { cond: Expr, then: Expr, else_: Expr },
    Let { name: Ident, expr: Expr, body: Expr },
    Lambda { param: Ident, expr: Expr },
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    name: InternedString,
    span: Span,
}

impl Ident {
    pub fn new(name: InternedString, span: Span) -> Self {
        Self { name, span }
    }

    pub fn name(&self) -> &InternedString {
        &self.name
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl ToString for Ident {
    fn to_string(&self) -> String {
        self.name.clone().to_string()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct UnaryOp {
    kind: UnaryOpKind,
    span: Span,
}

impl UnaryOp {
    pub fn new(kind: UnaryOpKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &UnaryOpKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl From<UnaryOp> for InternedString {
    fn from(op: UnaryOp) -> Self {
        InternedString::from(op.kind.to_string())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOpKind {
    Neg,
    Not,
}

impl From<Token> for UnaryOpKind {
    fn from(t: Token) -> Self {
        match t {
            Token::Minus => Self::Neg,
            Token::Bang => Self::Not,
            _ => unreachable!(),
        }
    }
}

impl ToString for UnaryOpKind {
    fn to_string(&self) -> String {
        match self {
            Self::Neg => "-".to_string(),
            Self::Not => "!".to_string(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct BinaryOp {
    kind: BinaryOpKind,
    span: Span,
}

impl BinaryOp {
    pub fn new(kind: BinaryOpKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &BinaryOpKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

impl From<BinaryOp> for InternedString {
    fn from(op: BinaryOp) -> Self {
        InternedString::from(op.kind.to_string())
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
    Eq,
    Neq,
    Lt,
    Lte,
    Gt,
    Gte,
    And,
    Or,
}

impl From<Token> for BinaryOpKind {
    fn from(t: Token) -> Self {
        match t {
            Token::Plus => Self::Add,
            Token::Minus => Self::Sub,
            Token::Star => Self::Mul,
            Token::Slash => Self::Div,
            Token::Percent => Self::Rem,
            Token::Caret => Self::Pow,
            Token::Eq => Self::Eq,
            Token::Neq => Self::Neq,
            Token::Lt => Self::Lt,
            Token::Leq => Self::Lte,
            Token::Gt => Self::Gt,
            Token::Geq => Self::Gte,
            Token::And => Self::And,
            Token::Or => Self::Or,
            _ => unreachable!(),
        }
    }
}

impl ToString for BinaryOpKind {
    fn to_string(&self) -> String {
        match self {
            Self::Add => "+".to_string(),
            Self::Sub => "-".to_string(),
            Self::Mul => "*".to_string(),
            Self::Div => "/".to_string(),
            Self::Rem => "%".to_string(),
            Self::Pow => "^".to_string(),
            Self::Eq => "==".to_string(),
            Self::Neq => "!=".to_string(),
            Self::Lt => "<".to_string(),
            Self::Lte => "<=".to_string(),
            Self::Gt => ">".to_string(),
            Self::Gte => ">=".to_string(),
            Self::And => "&&".to_string(),
            Self::Or => "||".to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lit {
    Num(Rational64),
    Bool(bool),
    String(InternedString),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    kind: DeclKind,
    span: Span,
}

impl Decl {
    pub fn new(kind: DeclKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn kind(&self) -> &DeclKind {
        &self.kind
    }

    pub fn span(&self) -> &Span {
        &self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Let { name: Ident, expr: Expr },
}

pub fn parse<'src>(src: &'src str) -> Result<Root, Vec<Rich<'src, Token, Span>>> {
    let tokens = Token::lexer(&src).spanned().map(|(tok, span)| match tok {
        Ok(tok) => (tok, Span::from(span)),
        Err(_) => (Token::Error, Span::from(span)),
    });
    let tok_stream = Stream::from_iter(tokens).spanned(Span::from(src.len()..src.len()));
    root_parser().parse(tok_stream).into_result()
}

fn root_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Root, extra::Err<Rich<'a, Token, Span>>> {
    decl_parser()
        .repeated()
        .collect()
        .map_with_span(Root::new)
        .boxed()
}

fn item_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Item, extra::Err<Rich<'a, Token, Span>>> {
    expr_parser()
        .map(ItemKind::Expr)
        .or(decl_parser().map(ItemKind::Decl))
        .map_with_span(Item::new)
        .boxed()
}

fn decl_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Decl, extra::Err<Rich<'a, Token, Span>>> {
    let let_ = just(Token::Let)
        .ignore_then(ident_parser())
        .then_ignore(just(Token::Assign))
        .then(expr_parser())
        .map(|(name, expr)| DeclKind::Let { name, expr });

    let fn_ = just(Token::Let)
        .ignore_then(ident_parser())
        .then(ident_parser().repeated().at_least(1).collect())
        .then_ignore(just(Token::Assign))
        .then(expr_parser())
        .map(|((name, params), expr)| DeclKind::Let {
            name: name.clone(),
            expr: curry_fn(params, expr),
        });

    let_.or(fn_).map_with_span(Decl::new).boxed()
}

fn expr_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Expr, extra::Err<Rich<'a, Token, Span>>> {
    recursive(|expr| {
        let unit = just(Token::LParen)
            .ignore_then(just(Token::RParen))
            .map(|_| ExprKind::Unit)
            .map_with_span(Expr::new);

        let lit = lit_parser().map(ExprKind::Lit).map_with_span(Expr::new);

        let let_ = just(Token::Let)
            .ignore_then(ident_parser())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((name, expr), body)| ExprKind::Let { name, expr, body })
            .map_with_span(Expr::new);

        let fn_ = just(Token::Let)
            .ignore_then(ident_parser())
            .then(ident_parser().repeated().at_least(1).collect())
            .then_ignore(just(Token::Assign))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|(((name, params), expr), body)| ExprKind::Let {
                name: name.clone(),
                expr: curry_fn(params, expr),
                body,
            })
            .map_with_span(Expr::new);

        let lambda = just(Token::Backslash)
            .ignore_then(ident_parser().repeated().at_least(1).collect())
            .then_ignore(just(Token::RArrow))
            .then(expr.clone())
            .map(|(params, expr)| curry_fn(params, expr));

        let if_ = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .then_ignore(just(Token::Else))
            .then(expr.clone())
            .map(|((cond, then), else_)| ExprKind::If { cond, then, else_ })
            .map_with_span(Expr::new);

        let atom = ident_parser()
            .map(ExprKind::Ident)
            .map_with_span(Expr::new)
            .or(unit)
            .or(lit)
            .or(let_)
            .or(fn_)
            .or(lambda)
            .or(if_)
            .or(expr
                .clone()
                .delimited_by(just(Token::LParen), just(Token::RParen)))
            .boxed();

        let apply = atom
            .clone()
            .then(atom.clone().repeated().collect::<Vec<_>>())
            .map(|(fun, args)| {
                if args.len() == 0 {
                    fun.clone()
                } else {
                    args.into_iter().fold(fun, |fun, arg| {
                        Expr::new(
                            ExprKind::Apply {
                                fun: fun.clone(),
                                arg: arg.clone(),
                            },
                            fun.span().extend(*arg.span()),
                        )
                    })
                }
            })
            .boxed();

        let op = just(Token::Minus)
            .map(UnaryOpKind::from)
            .or(just(Token::Bang).map(UnaryOpKind::from))
            .map_with_span(UnaryOp::new)
            .boxed();

        let unary = op
            .clone()
            .repeated()
            .foldr(apply.clone(), |op, expr| {
                Expr::new(
                    ExprKind::Unary {
                        op: op.clone(),
                        expr: expr.clone(),
                    },
                    op.span().extend(*expr.span()),
                )
            })
            .boxed();

        let op = just(Token::Caret)
            .map(BinaryOpKind::from)
            .map_with_span(BinaryOp::new)
            .boxed();

        let pow = unary
            .clone()
            .foldl(
                op.clone().then(unary.clone()).repeated(),
                |lhs: Expr, (op, rhs): (BinaryOp, Expr)| {
                    Expr::new(
                        ExprKind::Binary {
                            op: op.clone(),
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        lhs.span().extend(*rhs.span()),
                    )
                },
            )
            .boxed();

        let op = just(Token::Star)
            .map(BinaryOpKind::from)
            .or(just(Token::Slash).map(BinaryOpKind::from))
            .or(just(Token::Percent).map(BinaryOpKind::from))
            .map_with_span(BinaryOp::new)
            .boxed();

        let factor = pow
            .clone()
            .foldl(
                op.clone().then(pow.clone()).repeated(),
                |lhs: Expr, (op, rhs): (BinaryOp, Expr)| {
                    Expr::new(
                        ExprKind::Binary {
                            op: op.clone(),
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        lhs.span().extend(*rhs.span()),
                    )
                },
            )
            .boxed();

        let op = just(Token::Plus)
            .map(BinaryOpKind::from)
            .or(just(Token::Minus).map(BinaryOpKind::from))
            .map_with_span(BinaryOp::new)
            .boxed();

        let term = factor
            .clone()
            .foldl(
                op.clone().then(factor.clone()).repeated(),
                |lhs: Expr, (op, rhs): (BinaryOp, Expr)| {
                    Expr::new(
                        ExprKind::Binary {
                            op: op.clone(),
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        lhs.span().extend(*rhs.span()),
                    )
                },
            )
            .boxed();

        let op = just(Token::Lt)
            .map(BinaryOpKind::from)
            .or(just(Token::Gt).map(BinaryOpKind::from))
            .or(just(Token::Leq).map(BinaryOpKind::from))
            .or(just(Token::Geq).map(BinaryOpKind::from))
            .map_with_span(BinaryOp::new)
            .boxed();

        let cmp = term
            .clone()
            .then(op.clone().then(term.clone()).or_not())
            .map(|(lhs, rhs)| match rhs {
                Some((op, rhs)) => Expr::new(
                    ExprKind::Binary {
                        op,
                        lhs: lhs.clone(),
                        rhs: rhs.clone(),
                    },
                    lhs.span().extend(*rhs.span()),
                ),
                None => lhs.clone(),
            })
            .boxed();

        let op = just(Token::Eq)
            .map(BinaryOpKind::from)
            .or(just(Token::Neq).map(BinaryOpKind::from))
            .map_with_span(BinaryOp::new)
            .boxed();

        let eq = cmp
            .clone()
            .then(op.clone().then(cmp.clone()).or_not())
            .map(|(lhs, rhs)| match rhs {
                Some((op, rhs)) => Expr::new(
                    ExprKind::Binary {
                        op,
                        lhs: lhs.clone(),
                        rhs: rhs.clone(),
                    },
                    lhs.span().extend(*rhs.span()),
                ),
                None => lhs.clone(),
            })
            .boxed();

        let and = eq
            .clone()
            .foldl(
                just(Token::And)
                    .map(BinaryOpKind::from)
                    .map_with_span(BinaryOp::new)
                    .then(eq.clone())
                    .repeated(),
                |lhs: Expr, (op, rhs): (BinaryOp, Expr)| {
                    Expr::new(
                        ExprKind::Binary {
                            op: op.clone(),
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        lhs.span().extend(*rhs.span()),
                    )
                },
            )
            .boxed();

        let or = and
            .clone()
            .foldl(
                just(Token::Or)
                    .map(BinaryOpKind::from)
                    .map_with_span(BinaryOp::new)
                    .then(and.clone())
                    .repeated(),
                |lhs: Expr, (op, rhs): (BinaryOp, Expr)| {
                    Expr::new(
                        ExprKind::Binary {
                            op: op.clone(),
                            lhs: lhs.clone(),
                            rhs: rhs.clone(),
                        },
                        lhs.span().extend(*rhs.span()),
                    )
                },
            )
            .boxed();

        or
    })
}

fn ident_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Ident, extra::Err<Rich<'a, Token, Span>>> {
    select! {
        Token::Ident(name) => name
    }
    .map_with_span(|name, span| Ident::new(name, span))
}

fn lit_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
) -> impl ChumskyParser<'a, I, Lit, extra::Err<Rich<'a, Token, Span>>> {
    select! {
        Token::Num(n) => Lit::Num(n),
        Token::Bool(b) => Lit::Bool(b),
        Token::String(s) => Lit::String(s),
    }
}

fn curry_fn(params: Vec<Ident>, expr: Expr) -> Expr {
    params.into_iter().rev().fold(expr, |expr, param| {
        Expr::new(
            ExprKind::Lambda {
                param: param.clone(),
                expr: expr.clone(),
            },
            param.span().extend(*expr.span()),
        )
    })
}

mod tests {
    use crate::parse::parse;

    #[test]
    fn parse_let() {
        let src = "let x = 1";
        let root = parse(src).expect("Failed to parse");
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_arithmetic() {
        let src = "let a = 1 + 2/3 * 3^2 - 4 / 5 % 10";
        let root = parse(src).expect("Failed to parse");
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_boolean_cmp() {
        let src = "let a = 1 < 2 && 3 > 4 || 5 <= 6 && 7 >= 8 && !(9 == 10 && 11 != 12)";
        let root = parse(src).expect("Failed to parse");
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_if() {
        let src = "let a = if true then 1 else 2";
        let root = parse(src).expect("Failed to parse");
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_lambda() {
        let src = "let a = \\x -> x + 1";
        let root = parse(src).expect("Failed to parse");
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_lambda_apply() {
        let src = "let add = (\\x y -> x + y) 1 2";
        let root = parse(src).expect("Failed to parse");
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_let_fn() {
        let src = "let add x y = x + y";
        let root = parse(src).expect("Failed to parse");
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_let_fn_apply() {
        let src = "let f g x = f (g x)";
        let root = parse(src).expect("Failed to parse");
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_let_expr() {
        let src = "let x = let y = 1 in y + 1";
        let root = parse(src).expect("Failed to parse");
        insta::assert_debug_snapshot!(root);
    }

    #[test]
    fn parse_let_fn_expr() {
        let src = "let f x = let g y = x + y in g 1";
        let root = parse(src).expect("Failed to parse");
        insta::assert_debug_snapshot!(root);
    }
}
