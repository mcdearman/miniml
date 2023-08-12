use cstree::Syntax;

#[repr(u32)]
#[derive(Syntax, Debug, Clone, Copy, PartialEq, Eq)]
pub enum SyntaxKind {
    // Tokens / Terminals
    Error,
    Comment,
    Whitespace,
    Ident,
    // Keywords
    #[static_text("pub")]
    Pub,
    #[static_text("mod")]
    Module,
    #[static_text("end")]
    End,
    #[static_text("use")]
    Use,
    #[static_text("const")]
    Const,
    #[static_text("let")]
    Let,
    #[static_text("fn")]
    Fn,
    #[static_text("struct")]
    Struct,
    #[static_text("match")]
    Match,
    #[static_text("with")]
    With,
    #[static_text("and")]
    And,
    #[static_text("or")]
    Or,
    #[static_text("not")]
    Not,
    #[static_text("if")]
    If,
    #[static_text("elif")]
    Elif,
    #[static_text("then")]
    Then,
    #[static_text("else")]
    Else,
    #[static_text("in")]
    In,
    // Literals
    Char,
    String,
    Complex,
    Real,
    Rational,
    Int,
    // Punctuation
    #[static_text("+")]
    Plus,
    #[static_text("-")]
    Minus,
    #[static_text("*")]
    Star,
    #[static_text("/")]
    Slash,
    #[static_text("%")]
    Percent,
    #[static_text("^")]
    Caret,
    #[static_text("\\")]
    Backslash,
    #[static_text("->")]
    Arrow,
    #[static_text("=>")]
    FatArrow,
    #[static_text("|")]
    Pipe,
    #[static_text("|>")]
    PipeArrow,
    #[static_text("=")]
    Eq,
    #[static_text("<")]
    Lt,
    #[static_text(">")]
    Gt,
    #[static_text("!=")]
    Neq,
    #[static_text("<=")]
    Leq,
    #[static_text(">=")]
    Geq,
    #[static_text("(")]
    LParen,
    #[static_text(")")]
    RParen,
    // Nodes / Non-terminals
    Lit,
    IfExpr,
    Lambda,
    LetExpr,
    Atom,
    Apply,
    Unary,
    Power,
    Factor,
    Term,
    Expr,
    Root,
}

pub type Miniml = SyntaxKind;
