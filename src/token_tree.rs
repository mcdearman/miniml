#[derive(Debug, Clone)]
pub enum TokenTree {
    #[default]
    Error,
    #[regex(r"--.*", logos::skip)]
    Comment,
    #[regex(r" +")]
    Space,
    #[regex(r"\t+")]
    Tab,
    #[regex(r"[\n\r]+")]
    Newline,

    // Literals and identifiers
    // #[regex(r"-?0b(0|1)+(/-?(0|1)+)?", |lex| Rational64::from_str_radix(&lex.slice()[2..], 2).ok().map(Num))]
    // #[regex(r"-?0o[0-7]+(/-?[0-7]+)?", |lex| Rational64::from_str_radix(&lex.slice()[2..], 8).ok().map(Num))]
    // #[regex(r"-?0x[0-9a-fA-F]+(/-?[0-9a-fA-F]+)?", |lex| Rational64::from_str_radix(&lex.slice()[2..], 16).ok().map(Num))]
    #[regex(r"-?([1-9]\d*|0)(/-?-[1-9]\d*|0)?", |lex| lex.slice().parse().ok())]
    Num(Num),
    #[regex(r"true|false", |lex| lex.slice().parse().ok())]
    Bool(bool),
    #[regex(r#""(\\.|[^"\\])*""#, |lex| lex.slice().to_string())]
    String(String),
    // #[token(r#"f""#, format_string)]
    // FormatString(Vec<Self>),
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

    // Punctuation
    Lambda,
    Arrow,
    Assign,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Eq,
    Neq,
    Lt,
    Leq,
    Geq,
    Bang,
    LParen,
    RParen,
    RBrace,
    LBrack,
    RBrack,
    Pipe,
    PipeArrow,
    Subtype,

    Semicolon,
    Comma,
    Wildcard,

    // Keywords
    Class,
    Enum,
    Trait,
    Let,
    In,
    Match,
    With,
    If,
    Then,
    Else,
}
