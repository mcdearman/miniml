// ============================================================================
//                                UTILITIES
// ============================================================================

mod intern {
    use lasso::{Spur, ThreadedRodeo};
    use once_cell::sync::Lazy;
    use std::{
        borrow::Borrow,
        fmt::{Debug, Display},
        ops::Deref,
    };

    pub static mut INTERNER: Lazy<ThreadedRodeo> = Lazy::new(|| ThreadedRodeo::default());

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
    pub struct InternedString {
        key: Spur,
    }

    impl From<Spur> for InternedString {
        fn from(key: Spur) -> Self {
            Self { key }
        }
    }

    impl From<&str> for InternedString {
        fn from(name: &str) -> Self {
            Self {
                key: unsafe { INTERNER.get_or_intern(name) },
            }
        }
    }

    impl From<String> for InternedString {
        fn from(name: String) -> Self {
            Self {
                key: unsafe { INTERNER.get_or_intern(name) },
            }
        }
    }

    impl Debug for InternedString {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "InternedString({})", unsafe {
                INTERNER.resolve(&self.key)
            })
        }
    }

    impl Display for InternedString {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "\"{}\"", unsafe { INTERNER.resolve(&self.key) })
        }
    }

    impl Borrow<str> for InternedString {
        fn borrow(&self) -> &str {
            unsafe { INTERNER.resolve(&self.key) }
        }
    }

    impl Deref for InternedString {
        type Target = str;

        fn deref(&self) -> &Self::Target {
            unsafe { INTERNER.resolve(&self.key) }
        }
    }
}

mod list {
    use std::fmt::Display;

    #[derive(Debug, Clone, PartialEq, Default)]
    pub enum List<T> {
        #[default]
        Empty,
        Pair {
            head: T,
            tail: Box<Self>,
        },
    }

    impl<T> List<T> {
        pub fn head(&self) -> Option<&T> {
            match self {
                Self::Empty => None,
                Self::Pair { head, .. } => Some(head),
            }
        }

        pub fn tail(&self) -> Option<&Self> {
            match self {
                Self::Empty => None,
                Self::Pair { tail, .. } => Some(tail),
            }
        }

        pub fn push_front(&mut self, head: T) {
            let tail = std::mem::replace(self, Self::Empty);
            *self = Self::Pair {
                head,
                tail: Box::new(tail),
            };
        }

        pub fn push_back(&mut self, head: T) {
            let mut tail = self;
            loop {
                match tail {
                    Self::Empty => {
                        *tail = Self::Pair {
                            head,
                            tail: Box::new(Self::Empty),
                        };
                        break;
                    }
                    Self::Pair { tail: next, .. } => {
                        tail = next;
                    }
                }
            }
        }

        pub fn iter(&self) -> impl Iterator<Item = &T> {
            ListIter::new(self)
        }
    }

    impl<'a, T> Display for List<T>
    where
        T: Display,
    {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "(")?;
            for (i, s) in self.iter().enumerate() {
                if i != 0 {
                    write!(f, " ")?;
                }
                write!(f, "{}", s)?;
            }
            write!(f, ")")
        }
    }

    impl<T, I> From<I> for List<T>
    where
        I: IntoIterator<Item = T>,
        <I as IntoIterator>::IntoIter: DoubleEndedIterator,
    {
        fn from(value: I) -> Self {
            let mut list = List::Empty;
            for item in value.into_iter().rev() {
                list.push_front(item);
            }
            list
        }
    }

    #[derive(Debug)]
    struct ListIter<'a, T> {
        list: &'a List<T>,
    }

    impl<'a, T> ListIter<'a, T> {
        fn new(list: &'a List<T>) -> Self {
            Self { list }
        }
    }

    impl<'a, T> Iterator for ListIter<'a, T> {
        type Item = &'a T;

        fn next(&mut self) -> Option<Self::Item> {
            match self.list {
                List::Empty => None,
                List::Pair { head, tail } => {
                    self.list = tail;
                    Some(head)
                }
            }
        }
    }
}

mod span {
    use std::{
        fmt::{Debug, Display},
        ops::{Index, Range},
    };

    #[derive(Clone, Copy, Eq, PartialEq, Default, Hash)]
    pub struct Span {
        start: u32,
        end: u32,
    }

    impl Span {
        pub fn new(start: u32, end: u32) -> Self {
            Self { start, end }
        }

        pub fn start(&self) -> u32 {
            self.start
        }

        pub fn end(&self) -> u32 {
            self.end
        }

        pub fn extend(&self, other: Span) -> Self {
            Self {
                start: self.start.min(other.start),
                end: self.end.max(other.end),
            }
        }
    }

    impl Display for Span {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}..{}", self.start, self.end)
        }
    }

    impl Debug for Span {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{}..{}", self.start, self.end)
        }
    }

    impl From<Span> for Range<usize> {
        fn from(span: Span) -> Self {
            span.start as usize..span.end as usize
        }
    }

    impl From<Range<usize>> for Span {
        fn from(range: Range<usize>) -> Self {
            Self {
                start: range.start as u32,
                end: range.end as u32,
            }
        }
    }

    impl chumsky::span::Span for Span {
        type Context = ();

        type Offset = u32;

        fn new(context: Self::Context, range: Range<Self::Offset>) -> Self {
            Self {
                start: range.start,
                end: range.end,
            }
        }

        fn context(&self) -> Self::Context {
            ()
        }

        fn start(&self) -> Self::Offset {
            self.start
        }

        fn end(&self) -> Self::Offset {
            self.end
        }
    }

    impl Index<Span> for str {
        type Output = str;

        fn index(&self, index: Span) -> &Self::Output {
            &self[Range::from(index)]
        }
    }

    impl Index<Span> for String {
        type Output = str;

        fn index(&self, index: Span) -> &Self::Output {
            &self[Range::from(index)]
        }
    }
}

mod unique_id {
    use std::{
        fmt::{Debug, Display},
        sync::atomic::{AtomicUsize, Ordering},
    };

    #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct UniqueId(usize);

    impl UniqueId {
        pub fn new(id: usize) -> Self {
            Self(id)
        }
    }

    static COUNTER: AtomicUsize = AtomicUsize::new(0);

    impl UniqueId {
        pub fn gen() -> Self {
            Self(COUNTER.fetch_add(1, Ordering::SeqCst))
        }
    }

    impl Debug for UniqueId {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "UniqueId({})", self.0)
        }
    }

    impl Display for UniqueId {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "#{}", self.0)
        }
    }

    impl From<usize> for UniqueId {
        fn from(id: usize) -> Self {
            Self(id)
        }
    }

    impl From<UniqueId> for usize {
        fn from(id: UniqueId) -> Self {
            id.0
        }
    }

    impl PartialEq<usize> for UniqueId {
        fn eq(&self, other: &usize) -> bool {
            self.0 == *other
        }
    }

    impl PartialOrd<usize> for UniqueId {
        fn partial_cmp(&self, other: &usize) -> Option<std::cmp::Ordering> {
            self.0.partial_cmp(other)
        }
    }
}

// ============================================================================
//                             LEXICAL ANALYSIS
// ============================================================================

mod token {
    use crate::intern::InternedString;
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
        #[token("_")]
        Wildcard,
        #[token("\\")]
        Backslash,
        #[token("<-")]
        LArrow,
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
        #[token("or")]
        Or,
        #[token("and")]
        And,
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
        #[token(":")]
        Colon,
        #[token("::")]
        DoubleColon,
        #[token(";")]
        SemiColon,
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
        #[token("|")]
        Bar,
        #[token("|>")]
        Pipe,

        // Keywords
        #[token("pub")]
        Pub,
        #[token("mod")]
        Mod,
        #[token("end")]
        End,
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
        #[token("type")]
        Type,
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
                Wildcard => write!(f, "Wildcard"),
                Backslash => write!(f, "Backslash"),
                LArrow => write!(f, "LArrow"),
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
                Neq => write!(f, "Neq"),
                Lt => write!(f, "Lt"),
                Gt => write!(f, "Gt"),
                Leq => write!(f, "Leq"),
                Geq => write!(f, "Geq"),
                Bang => write!(f, "Bang"),
                Comma => write!(f, "Comma"),
                Colon => write!(f, "Colon"),
                DoubleColon => write!(f, "DoubleColon"),
                SemiColon => write!(f, "SemiColon"),
                LParen => write!(f, "LParen"),
                RParen => write!(f, "RParen"),
                LBrace => write!(f, "LBrace"),
                RBrace => write!(f, "RBrace"),
                LBrack => write!(f, "LBrack"),
                RBrack => write!(f, "RBrack"),
                Bar => write!(f, "Bar"),
                Pipe => write!(f, "Pipe"),
                Pub => write!(f, "Pub"),
                Mod => write!(f, "Mod"),
                End => write!(f, "End"),
                Let => write!(f, "Let"),
                In => write!(f, "In"),
                If => write!(f, "If"),
                Then => write!(f, "Then"),
                Else => write!(f, "Else"),
                Type => write!(f, "Type"),
            }
        }
    }
}

// ============================================================================
//                                PARSING
// ============================================================================

mod ast {
    use crate::{intern::InternedString, list::List, span::Span, token::Token};
    use num_rational::Rational64;

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
        DataType(DataType),
        Let { pattern: Pattern, expr: Expr },
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct DataType {
        name: Ident,
        kind: Box<DataTypeKind>,
        span: Span,
    }

    impl DataType {
        pub fn new(name: Ident, kind: DataTypeKind, span: Span) -> Self {
            Self {
                name,
                kind: Box::new(kind),
                span,
            }
        }

        pub fn name(&self) -> &Ident {
            &self.name
        }

        pub fn kind(&self) -> &DataTypeKind {
            &self.kind
        }

        pub fn span(&self) -> &Span {
            &self.span
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum DataTypeKind {
        Record {
            fields: Vec<(Ident, TypeHint)>,
        },
        Sum {
            variants: Vec<(Ident, Option<TypeHint>)>,
        },
        Product {
            fields: Vec<TypeHint>,
        },
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
        Apply {
            fun: Expr,
            arg: Expr,
        },
        Unary {
            op: UnaryOp,
            expr: Expr,
        },
        Binary {
            op: BinaryOp,
            lhs: Expr,
            rhs: Expr,
        },
        If {
            cond: Expr,
            then: Expr,
            else_: Expr,
        },
        Let {
            pattern: Pattern,
            expr: Expr,
            body: Expr,
        },
        List(List<Expr>),
        Tuple(Vec<Expr>),
        ListComp {
            expr: Expr,
            pattern: Pattern,
            list: Expr,
            filter: Option<Expr>,
        },
        Range {
            start: Expr,
            end: Expr,
            step: Option<Expr>,
        },
        Lambda {
            param: Pattern,
            expr: Expr,
        },
        FieldAccess {
            record: Expr,
            field: Ident,
        },
        Unit,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct TypeHint {
        kind: Box<TypeHintKind>,
        span: Span,
    }

    impl TypeHint {
        pub fn new(kind: TypeHintKind, span: Span) -> Self {
            Self {
                kind: Box::new(kind),
                span,
            }
        }

        pub fn kind(&self) -> &TypeHintKind {
            &self.kind
        }

        pub fn span(&self) -> &Span {
            &self.span
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum TypeHintKind {
        Num,
        Bool,
        String,
        Ident(Ident),
        List(TypeHint),
        Vec(Vec<TypeHint>),
        Tuple(Vec<TypeHint>),
        Fn(TypeHint, TypeHint),
        Unit,
    }

    #[derive(Debug, Clone, PartialEq)]
    pub struct Pattern {
        kind: PatternKind,
        span: Span,
    }

    impl Pattern {
        pub fn new(kind: PatternKind, span: Span) -> Self {
            Self { kind, span }
        }

        pub fn kind(&self) -> &PatternKind {
            &self.kind
        }

        pub fn span(&self) -> &Span {
            &self.span
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
                Token::Assign => Self::Eq,
                Token::Plus => Self::Add,
                Token::Minus => Self::Sub,
                Token::Star => Self::Mul,
                Token::Slash => Self::Div,
                Token::Percent => Self::Rem,
                Token::Caret => Self::Pow,
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
    pub enum PatternKind {
        Wildcard,
        Ident(Ident),
        Num(Rational64),
        Bool(bool),
        String(InternedString),
        Tuple(Vec<Pattern>),
        List(Vec<Pattern>),
        Cons(Vec<Pattern>, Box<Pattern>),
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

    #[derive(Debug, Clone, PartialEq)]
    pub enum Lit {
        Num(Rational64),
        Bool(bool),
        String(InternedString),
    }
}

mod parse {
    use crate::{ast::*, span::Span, token::Token};
    use chumsky::{
        error::Rich,
        extra,
        input::{Input, Stream, ValueInput},
        primitive::just,
        recursive::recursive,
        select, IterParser, Parser as ChumskyParser,
    };
    use logos::Logos;

    pub fn parse<'src>(
        tokens: Vec<Token>,
        repl: bool,
    ) -> (Option<Root>, Vec<Rich<'src, Token, Span>>) {
        // let tokens = Token::lexer(&src).spanned().map(|(tok, span)| match tok {
        //     Ok(tok) => (tok, Span::from(span)),
        //     Err(_) => (Token::Error, Span::from(span)),
        // });
        let tok_stream = Stream::from_iter(tokens).spanned(Span::from(src.len()..src.len()));
        if repl {
            repl_parser().parse(tok_stream).into_output_errors()
        } else {
            root_parser().parse(tok_stream).into_output_errors()
        }
    }

    fn root_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
    ) -> impl ChumskyParser<'a, I, Root, extra::Err<Rich<'a, Token, Span>>> {
        decl_parser()
            .repeated()
            .collect()
            .map_with_span(Root::new)
            .boxed()
    }

    fn repl_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
    ) -> impl ChumskyParser<'a, I, Root, extra::Err<Rich<'a, Token, Span>>> {
        expr_parser()
            .map(|e| DeclKind::Let {
                pattern: Pattern::new(
                    PatternKind::Ident(Ident::new(InternedString::from("main"), e.span().clone())),
                    e.span().clone(),
                ),
                expr: Expr::new(
                    ExprKind::Lambda {
                        param: Pattern::new(
                            PatternKind::Ident(Ident::new(
                                InternedString::from("args"),
                                e.span().clone(),
                            )),
                            e.span().clone(),
                        ),
                        expr: e.clone(),
                    },
                    e.span().clone(),
                ),
            })
            .map_with_span(Decl::new)
            .or(decl_parser())
            .map_with_span(|decl, span| Root::new(vec![decl], span))
            .boxed()
    }

    fn decl_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
    ) -> impl ChumskyParser<'a, I, Decl, extra::Err<Rich<'a, Token, Span>>> {
        let let_ = just(Token::Let)
            .ignore_then(pattern_parser())
            .then_ignore(just(Token::Assign))
            .then(expr_parser())
            .map(|(pattern, expr)| DeclKind::Let { pattern, expr });

        let fn_ = just(Token::Let)
            .ignore_then(ident_parser())
            .then(pattern_parser().repeated().at_least(1).collect())
            .then_ignore(just(Token::Assign))
            .then(expr_parser())
            .map(|((name, params), expr)| DeclKind::Let {
                pattern: Pattern::new(
                    PatternKind::Ident(name.clone()),
                    name.span().extend(*expr.span()),
                ),
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
                .ignore_then(pattern_parser())
                .then_ignore(just(Token::Assign))
                .then(expr.clone())
                .then_ignore(just(Token::In))
                .then(expr.clone())
                .map(|((pattern, expr), body)| ExprKind::Let {
                    pattern,
                    expr,
                    body,
                })
                .map_with_span(Expr::new);

            let fn_ = just(Token::Let)
                .ignore_then(ident_parser())
                .then(pattern_parser().repeated().at_least(1).collect())
                .then_ignore(just(Token::Assign))
                .then(expr.clone())
                .then_ignore(just(Token::In))
                .then(expr.clone())
                .map(|(((name, params), expr), body)| ExprKind::Let {
                    pattern: Pattern::new(
                        PatternKind::Ident(name.clone()),
                        name.span().extend(*body.span()),
                    ),
                    expr: curry_fn(params, expr),
                    body,
                })
                .map_with_span(Expr::new);

            let lambda = just(Token::Backslash)
                .ignore_then(pattern_parser().repeated().at_least(1).collect())
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

            let op = just(Token::Assign)
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

    fn pattern_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
    ) -> impl ChumskyParser<'a, I, Pattern, extra::Err<Rich<'a, Token, Span>>> {
        just(Token::Wildcard)
            .map_with_span(|_, span| Pattern::new(PatternKind::Wildcard, span))
            .or(ident_parser()
                .map_with_span(|ident, span| Pattern::new(PatternKind::Ident(ident), span)))
    }

    fn type_hint_parser<'a, I: ValueInput<'a, Token = Token, Span = Span>>(
    ) -> impl ChumskyParser<'a, I, TypeHint, extra::Err<Rich<'a, Token, Span>>> {
        ident_parser()
            .map(TypeHintKind::Ident)
            .map_with_span(TypeHint::new)
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

    fn curry_fn(params: Vec<Pattern>, expr: Expr) -> Expr {
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
            let (root, errors) = parse(src, false);
            insta::assert_debug_snapshot!(root);
        }

        #[test]
        fn parse_arithmetic() {
            let src = "let a = 1 + 2/3 * 3^2 - 4 / 5 % 10";
            let (root, errors) = parse(src, false);
            insta::assert_debug_snapshot!(root);
        }

        #[test]
        fn parse_boolean_cmp() {
            let src = "let a = 1 < 2 && 3 > 4 || 5 <= 6 && 7 >= 8 && !(9 == 10 && 11 != 12)";
            let (root, errors) = parse(src, false);
            insta::assert_debug_snapshot!(root);
        }

        #[test]
        fn parse_if() {
            let src = "let a = if true then 1 else 2";
            let (root, errors) = parse(src, false);
            insta::assert_debug_snapshot!(root);
        }

        #[test]
        fn parse_lambda() {
            let src = "let a = \\x -> x + 1";
            let (root, errors) = parse(src, false);
            insta::assert_debug_snapshot!(root);
        }

        #[test]
        fn parse_lambda_apply() {
            let src = "let add = (\\x y -> x + y) 1 2";
            let (root, errors) = parse(src, false);
            insta::assert_debug_snapshot!(root);
        }

        #[test]
        fn parse_let_fn() {
            let src = "let add x y = x + y";
            let (root, errors) = parse(src, false);
            insta::assert_debug_snapshot!(root);
        }

        #[test]
        fn parse_let_fn_apply() {
            let src = "let f g x = f (g x)";
            let (root, errors) = parse(src, false);
            insta::assert_debug_snapshot!(root);
        }

        #[test]
        fn parse_let_expr() {
            let src = "let x = let y = 1 in y + 1";
            let (root, errors) = parse(src, false);
            insta::assert_debug_snapshot!(root);
        }

        #[test]
        fn parse_let_fn_expr() {
            let src = "let f x = let g y = x + y in g 1";
            let (root, errors) = parse(src, false);
            insta::assert_debug_snapshot!(root);
        }
    }
}

// ============================================================================
//                             MAIN ENTRY POINT
// ============================================================================

fn main() {
    env_logger::init();
    // let mut src = String::new();
    // loop {
    //     print!("> ");
    //     io::stdout().flush().unwrap();
    //     io::stdin()
    //         .read_line(&mut src)
    //         .expect("Failed to read line");
    //     match src.trim() {
    //         // "db" => {
    //         //     println!("db: {:?}\n", db.clone());
    //         //     src.clear();
    //         //     continue;
    //         // }
    //         // "ast" => {
    //         //     println!("ast: {:?}\n", interpreter.ast());
    //         //     src.clear();
    //         //     continue;
    //         // }
    //         // "res_env" => {
    //         //     println!("res: {:?}\n", interpreter.res_env());
    //         //     src.clear();
    //         //     continue;
    //         // }
    //         "exit" => break,
    //         _ => (),
    //     }
    //     let root = match parse(&src, true) {
    //         (Some(root), errors) => {
    //             if errors.is_empty() {
    //                 root
    //             } else {
    //                 println!("Errors: {:#?}", errors);
    //                 src.clear();
    //                 continue;
    //             }
    //         }
    //         (None, errors) => {
    //             println!("Errors: {:#?}", errors);
    //             src.clear();
    //             continue;
    //         }
    //     };
    //     println!("root: {:#?}\n", root);
    //     // let
    //     // match interpreter.eval(&*src) {
    //     //     Ok(obj) => println!(""),
    //     //     Err(err) => println!("Error: {:?}", err),
    //     // }
    //     // log::trace!("src: {:?}", src);
    //     // match compiler.compile(&*src) {
    //     //     Ok(chunk) => match vm.exec(chunk) {
    //     //         Ok(obj) => println!("{}\n", obj),
    //     //         Err(err) => println!("Error: {:?}", err),
    //     //     },
    //     //     Err(err) => println!("Error: {:?}", err),
    //     // }
    //     io::stdout().flush().unwrap();
    //     src.clear();
    // }
}
