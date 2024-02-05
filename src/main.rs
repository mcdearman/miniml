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
    use crate::{ast::*, intern::InternedString, span::Span, token::Token};
    use chumsky::{
        error::Rich,
        extra,
        input::{Input, Stream, ValueInput},
        primitive::just,
        recursive::recursive,
        select, IterParser, Parser as ChumskyParser,
    };

    pub fn parse<'src>(
        tokens: impl Iterator<Item = (Token, Span)> + Clone + 'src,
        repl: bool,
    ) -> (Option<Root>, Vec<Rich<'src, Token, Span>>) {
        let eof_span = tokens
            .clone()
            .last()
            .map(|(_, span)| span)
            .unwrap_or_default();
        let tok_stream = Stream::from_iter(tokens).spanned(eof_span);
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
        use crate::{parse::parse, span::Span, token::Token};
        use itertools::Itertools;
        use logos::Logos;

        fn test_helper(src: &str) {
            let tokens = Token::lexer(src)
                .spanned()
                .map(|(res, s)| match res {
                    Ok(tok) => (tok, Span::from(s)),
                    Err(_) => panic!("lexing error"),
                })
                .collect_vec()
                .into_iter();
            let (root, errors) = parse(tokens, true);
            if errors.len() > 0 {
                panic!("parse errors: {:?}", errors);
            }
            insta::assert_debug_snapshot!(root);
        }

        #[test]
        fn parse_let() {
            test_helper("let x = 1");
        }

        #[test]
        fn parse_arithmetic() {
            test_helper("1 + 2/3 * 3^2 - 4 / 5 % 10");
        }

        #[test]
        fn parse_boolean_cmp() {
            test_helper("1 < 2 and 3 > 4 or 5 <= 6 and 7 >= 8 and !(9 = 10 and 11 != 12)")
        }

        #[test]
        fn parse_if() {
            test_helper("if true then 1 else 2");
        }

        #[test]
        fn parse_lambda() {
            test_helper("\\x -> x + 1");
        }

        #[test]
        fn parse_lambda_apply() {
            test_helper("(\\x y -> x + y) 1 2");
        }

        #[test]
        fn parse_let_fn() {
            test_helper("let add x y = x + y");
        }

        #[test]
        fn parse_let_fn_apply() {
            test_helper("let f g x = f (g x)");
        }

        #[test]
        fn parse_let_expr() {
            test_helper("let x = let y = 1 in y + 1");
        }

        #[test]
        fn parse_let_fn_expr() {
            test_helper("let f x = let g y = x + y in g 1");
        }
    }
}

// ============================================================================
//                              NAME RESOLUTION
// ============================================================================

mod rename {
    use crate::{intern::InternedString, span::Span, unique_id::UniqueId};
    use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

    #[derive(Debug, Clone, PartialEq)]
    pub struct ResError {
        kind: ResErrorKind,
        span: Span,
    }

    impl ResError {
        pub fn new(kind: ResErrorKind, span: Span) -> Self {
            Self { kind, span }
        }

        pub fn kind(&self) -> &ResErrorKind {
            &self.kind
        }

        pub fn span(&self) -> &Span {
            &self.span
        }
    }

    impl Display for ResError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(f, "{:?} @ {}", self.span, self.kind)
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum ResErrorKind {
        UnboundName(InternedString),
    }

    impl Display for ResErrorKind {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                ResErrorKind::UnboundName(name) => {
                    write!(f, "unbound name '{}'", name)
                }
            }
        }
    }

    pub type ResResult<T> = Result<T, ResError>;

    #[derive(Debug, Clone, PartialEq)]
    pub struct Env {
        parent: Option<Rc<RefCell<Env>>>,
        data: HashMap<InternedString, UniqueId>,
    }

    impl Env {
        pub fn new() -> Rc<RefCell<Self>> {
            Rc::new(RefCell::new(Self {
                parent: None,
                data: HashMap::new(),
            }))
        }

        pub fn new_with_parent(parent: Rc<RefCell<Self>>) -> Rc<RefCell<Self>> {
            Rc::new(RefCell::new(Self {
                parent: Some(parent),
                data: HashMap::new(),
            }))
        }

        fn find(&self, name: &InternedString) -> Option<UniqueId> {
            if let Some(id) = self.data.get(name) {
                Some(*id)
            } else if let Some(parent) = &self.parent {
                parent.borrow().find(name)
            } else {
                None
            }
        }

        fn find_in_scope(&self, name: &InternedString) -> Option<UniqueId> {
            self.data.get(name).copied()
        }

        fn find_in_parent(&self, name: &InternedString) -> Option<UniqueId> {
            if let Some(parent) = &self.parent {
                parent.borrow().find(name)
            } else {
                None
            }
        }

        pub fn define(&mut self, name: InternedString) -> UniqueId {
            let id = UniqueId::gen();
            self.data.insert(name, id);
            id
        }

        // pub fn insert(&mut self, name: InternedString, id: UniqueId) {
        //     self.data.insert(name, id);
        // }

        fn define_if_absent_in_scope(&mut self, name: InternedString) -> Option<UniqueId> {
            if let Some(id) = self.find_in_scope(&name) {
                Some(id)
            } else {
                let id = UniqueId::gen();
                self.data.insert(name, id);
                Some(id)
            }
        }

        fn define_if_absent(&mut self, name: InternedString) -> UniqueId {
            if let Some(id) = self.find(&name) {
                id
            } else {
                let id = UniqueId::gen();
                self.data.insert(name, id);
                id
            }
        }
    }

    #[derive(Debug)]
    pub struct Resolver {
        builtins: HashMap<UniqueId, InternedString>,
    }

    impl Resolver {
        pub fn new(db: Rc<RefCell<Database>>) -> Self {
            Self { db }
        }

        pub fn resolve(
            &mut self,
            env: Rc<RefCell<Env>>,
            root: &parse::Root,
        ) -> (Option<Root>, Vec<ResError>) {
            let mut errors = vec![];
            let mut decls = vec![];
            for decl in root.decls() {
                match self.resolve_decl(env.clone(), decl) {
                    Ok(d) => {
                        // trace!("env: {:#?}", env.borrow());
                        println!("resolver env: {:#?}", env.borrow());
                        decls.push(d);
                    }
                    Err(err) => {
                        trace!("env: {:#?}", env.borrow());
                        errors.push(err)
                    }
                }
            }
            if decls.is_empty() {
                (None, errors)
            } else {
                (
                    Some(Root {
                        decls,
                        span: root.span().clone(),
                    }),
                    errors,
                )
            }
        }

        fn resolve_decl(&mut self, env: Rc<RefCell<Env>>, decl: &parse::Decl) -> ResResult<Decl> {
            trace!("decl env: {:#?}", env.borrow());
            match decl.kind() {
                parse::DeclKind::Let { name, expr } => match expr.kind() {
                    parse::ExprKind::Lambda { .. } => {
                        let name =
                            Ident::new(env.borrow_mut().define(name.name().clone()), *name.span());
                        let let_env = Env::new_with_parent(env.clone());
                        let expr = self.resolve_expr(let_env.clone(), expr)?;
                        Ok(Decl::new(DeclKind::Let { name, expr }, decl.span().clone()))
                    }
                    _ => {
                        let let_env = Env::new_with_parent(env.clone());
                        let expr = self.resolve_expr(let_env.clone(), expr)?;
                        let name =
                            Ident::new(env.borrow_mut().define(name.name().clone()), *name.span());
                        Ok(Decl::new(DeclKind::Let { name, expr }, decl.span().clone()))
                    }
                },
            }
        }

        fn resolve_expr(&mut self, env: Rc<RefCell<Env>>, expr: &parse::Expr) -> ResResult<Expr> {
            match expr.kind() {
                parse::ExprKind::Lit(l) => match l {
                    parse::Lit::Num(n) => {
                        Ok(Expr::new(ExprKind::Lit(Lit::Num(n.clone())), *expr.span()))
                    }
                    parse::Lit::Bool(b) => {
                        Ok(Expr::new(ExprKind::Lit(Lit::Bool(*b)), expr.span().clone()))
                    }
                    parse::Lit::String(s) => Ok(Expr::new(
                        ExprKind::Lit(Lit::String(s.clone())),
                        *expr.span(),
                    )),
                },
                parse::ExprKind::Ident(ident) => {
                    if let Some(name) = env.borrow().find(ident.name()) {
                        Ok(Expr::new(
                            ExprKind::Ident(Ident::new(name, expr.span().clone())),
                            expr.span().clone(),
                        ))
                    } else {
                        Err(ResError::new(
                            ResErrorKind::UnboundName(*ident.name()),
                            *expr.span(),
                        ))
                    }
                }
                parse::ExprKind::Apply { fun, arg } => Ok(Expr::new(
                    ExprKind::Apply {
                        fun: self.resolve_expr(env.clone(), fun)?,
                        arg: self.resolve_expr(env.clone(), arg)?,
                    },
                    *expr.span(),
                )),
                parse::ExprKind::Unary { op, expr } => {
                    let id = env.borrow_mut().define_if_absent(InternedString::from(*op));
                    let ident = Ident::new(id, *op.span());
                    self.db
                        .borrow_mut()
                        .insert_or_get(id, InternedString::from(*op));
                    Ok(Expr::new(
                        ExprKind::Apply {
                            fun: Expr::new(ExprKind::Ident(ident.clone()), *op.span()),
                            arg: self.resolve_expr(env.clone(), expr)?,
                        },
                        *expr.span(),
                    ))
                }
                parse::ExprKind::Binary { op, lhs, rhs } => {
                    let id = env.borrow_mut().define_if_absent(InternedString::from(*op));
                    let ident = Ident::new(id, *op.span());
                    self.db
                        .borrow_mut()
                        .insert_or_get(id, InternedString::from(*op));
                    Ok(Expr::new(
                        ExprKind::Apply {
                            fun: Expr::new(
                                ExprKind::Apply {
                                    fun: Expr::new(ExprKind::Ident(ident.clone()), *op.span()),
                                    arg: self.resolve_expr(env.clone(), lhs)?,
                                },
                                *op.span(),
                            ),
                            arg: self.resolve_expr(env.clone(), rhs)?,
                        },
                        *expr.span(),
                    ))
                }
                parse::ExprKind::If { cond, then, else_ } => Ok(Expr::new(
                    ExprKind::If {
                        cond: self.resolve_expr(env.clone(), cond)?,
                        then: self.resolve_expr(env.clone(), then)?,
                        else_: self.resolve_expr(env.clone(), else_)?,
                    },
                    *expr.span(),
                )),
                parse::ExprKind::Let { name, expr, body } => match expr.kind() {
                    parse::ExprKind::Lambda { .. } => {
                        let name =
                            Ident::new(env.borrow_mut().define(name.name().clone()), *name.span());
                        let let_env = Env::new_with_parent(env.clone());
                        let res_expr = self.resolve_expr(let_env.clone(), &expr)?;
                        let body = self.resolve_expr(let_env.clone(), &body)?;
                        Ok(Expr::new(
                            ExprKind::Let {
                                name,
                                expr: res_expr,
                                body,
                            },
                            *expr.span(),
                        ))
                    }
                    _ => {
                        let let_env = Env::new_with_parent(env.clone());
                        let res_expr = self.resolve_expr(let_env.clone(), expr)?;
                        let name =
                            Ident::new(env.borrow_mut().define(name.name().clone()), *name.span());
                        let body = self.resolve_expr(let_env.clone(), body)?;
                        Ok(Expr::new(
                            ExprKind::Let {
                                name,
                                expr: res_expr,
                                body,
                            },
                            *expr.span(),
                        ))
                    }
                },
                parse::ExprKind::Lambda { param, expr } => {
                    let lam_env = Env::new_with_parent(env.clone());
                    let param =
                        Ident::new(env.borrow_mut().define(param.name().clone()), *param.span());
                    Ok(Expr::new(
                        ExprKind::Lambda {
                            param,
                            expr: self.resolve_expr(lam_env.clone(), expr)?,
                        },
                        *expr.span(),
                    ))
                }
                parse::ExprKind::Unit => Ok(Expr::new(ExprKind::Unit, *expr.span())),
            }
        }
    }

    mod tests {
        use super::{Env, Resolver};
        use crate::{db::Database, parse::parse};
        use std::{cell::RefCell, rc::Rc};

        fn test_helper(src: &str) -> super::Root {
            if let (Some(ast), errors) = parse(src) {
                let db = Rc::new(RefCell::new(Database::new()));
                let mut resolver = Resolver::new(db.clone());
                let (res, errors) = resolver.resolve(Env::new(), &ast);
                if !errors.is_empty() {
                    panic!("resolve error: {:?}", errors);
                }
                res.unwrap()
            } else {
                panic!("parse error");
            }
        }

        #[test]
        fn res_let() {
            insta::assert_debug_snapshot!(test_helper("let x = 1"));
        }

        #[test]
        fn res_let_error() {
            let src = "let x = x";
            if let (Some(ast), errors) = parse(src) {
                let db = Rc::new(RefCell::new(Database::new()));
                let mut r = Resolver::new(db.clone());
                let (_, errors) = r.resolve(Env::new(), &ast);
                assert!(!errors.is_empty());
            } else {
                panic!("parse error");
            }
        }

        #[test]
        fn res_nested() {
            insta::assert_debug_snapshot!(test_helper("let a = let x = 1 in let y = 2 in x + y"));
        }

        // #[test]
        // fn res_fn_def() {
        //     let (ast, errors) = crate::syntax::parse::parse("add x y = x + y");
        //     if !errors.is_empty() {
        //         panic!("parse error: {:?}", errors);
        //     }
        //     let (res, errors) = super::resolve(&ast.unwrap());
        //     if !errors.is_empty() {
        //         panic!("resolve error: {:?}", errors);
        //     }
        //     insta::assert_debug_snapshot!(res);
        // }

        // #[test]
        // fn res_rec_fn_def() {
        //     let (ast, errors) = crate::syntax::parse::parse("f x = f x");
        //     if !errors.is_empty() {
        //         panic!("parse error: {:?}", errors);
        //     }
        //     let (res, errors) = super::resolve(&ast.unwrap());
        //     if !errors.is_empty() {
        //         panic!("resolve error: {:?}", errors);
        //     }
        //     insta::assert_debug_snapshot!(res);
        // }

        // #[test]
        // fn res_rec_let() {
        //     let (ast, errors) = crate::syntax::parse::parse("let f x = f x in f 1");
        //     if !errors.is_empty() {
        //         panic!("parse error: {:?}", errors);
        //     }
        //     let (res, errors) = super::resolve(&ast.unwrap());
        //     if !errors.is_empty() {
        //         panic!("resolve error: {:?}", errors);
        //     }
        //     insta::assert_debug_snapshot!(res);
        // }
    }
}

// ============================================================================
//                                EVALUATION
// ============================================================================

mod eval {
    use crate::{intern::InternedString, rename::ResError, unique_id::UniqueId};
    use num_rational::Rational64;
    use std::{
        cell::RefCell,
        collections::HashMap,
        fmt::{Debug, Display},
        rc::Rc,
    };

    #[derive(Debug, Clone, PartialEq)]
    pub enum RuntimeError {
        ParseError(Vec<InternedString>),
        ResError(Vec<ResError>),
        ArityError(usize, usize),
        TypeError(InternedString),
        DivisionByZero,
    }

    impl Display for RuntimeError {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                RuntimeError::ParseError(errs) => {
                    write!(f, "Parse error:\n")?;
                    for err in errs {
                        write!(f, "{}\n", err)?;
                    }
                    Ok(())
                }
                RuntimeError::ResError(errs) => {
                    write!(f, "Resolve error:\n")?;
                    for err in errs {
                        write!(f, "{}\n", err)?;
                    }
                    Ok(())
                }
                RuntimeError::ArityError(expected, found) => {
                    write!(
                        f,
                        "Arity error: expected {} args, found {}",
                        expected, found
                    )
                }
                RuntimeError::TypeError(err) => write!(f, "Type error: {}", err),
                RuntimeError::DivisionByZero => write!(f, "Division by zero"),
            }
        }
    }

    pub type RuntimeResult<T> = Result<T, RuntimeError>;

    #[derive(Clone, PartialEq)]
    pub struct Env {
        parent: Option<Rc<RefCell<Env>>>,
        bindings: HashMap<UniqueId, Value>,
    }

    impl Env {
        pub fn new() -> Rc<RefCell<Self>> {
            Rc::new(RefCell::new(Self {
                parent: None,
                bindings: HashMap::new(),
            }))
        }

        pub fn new_with_parent(parent: Rc<RefCell<Env>>) -> Rc<RefCell<Self>> {
            Rc::new(RefCell::new(Self {
                parent: Some(parent),
                bindings: HashMap::new(),
            }))
        }

        pub fn insert(&mut self, id: UniqueId, value: Value) {
            self.bindings.insert(id, value);
        }

        pub fn get(&self, id: &UniqueId) -> Option<Value> {
            self.bindings.get(id).cloned().or(self
                .parent
                .as_ref()
                .and_then(|parent| parent.borrow().get(id).clone()))
        }
    }

    impl Debug for Env {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let mut builder = f.debug_struct("Env");
            builder.field(
                "parent",
                if let Some(_) = &self.parent {
                    &"Some"
                } else {
                    &"None"
                },
            );
            builder.field("bindings", &self.bindings);
            builder.finish()
        }
    }

    pub fn default_env(ops: HashMap<InternedString, UniqueId>) -> Rc<RefCell<Env>> {
        let env = Env::new();
        env.borrow_mut().insert(
            ops.get(&InternedString::from("+")).unwrap().clone(),
            Value::NativeFn(|args| {
                if args.len() != 2 {
                    return Err(RuntimeError::ArityError(2, args.len()).into());
                } else {
                    match (args.get(0).unwrap(), args.get(1).unwrap()) {
                        (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
                            Ok(Value::Lit(Lit::Num(l + r)))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError(InternedString::from(format!(
                                "Expected numbers got {:?}",
                                args
                            ))));
                        }
                    }
                }
            }),
        );
        // env.borrow_mut().insert(
        //     ops.get(&InternedString::from("-")).unwrap().clone(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             return Err(RuntimeError::ArityError(2, args.len()).into());
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
        //                     Ok(Value::Lit(Lit::Num(l - r)))
        //                 }
        //                 _ => {
        //                     return Err(format!("Expected number, found {:?}", args).into());
        //                 }
        //             }
        //         }
        //     }),
        // );
        // env.borrow_mut().insert(
        //     ops.get(&InternedString::from("*")).unwrap().clone(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             return Err(format!("Expected 2 args, found {}", args.len()).into());
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
        //                     Ok(Value::Lit(Lit::Num(l * r)))
        //                 }
        //                 _ => {
        //                     return Err(format!("Expected number, found {:?}", args).into());
        //                 }
        //             }
        //         }
        //     }),
        // );
        // env.borrow_mut().insert(
        //     ops.get(&InternedString::from("/")).unwrap().clone(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             return Err(format!("Expected 2 args, found {}", args.len()).into());
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
        //                     if r == &Rational64::from_integer(0) {
        //                         return Err(format!("Division by zero").into());
        //                     }
        //                     Ok(Value::Lit(Lit::Num(l / r)))
        //                 }
        //                 _ => {
        //                     return Err(format!("Expected number, found {:?}", args).into());
        //                 }
        //             }
        //         }
        //     }),
        // );
        // env.borrow_mut().insert(
        //     ops.get(&InternedString::from("%")).unwrap().clone(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             return Err(format!("Expected 2 args, found {}", args.len()).into());
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
        //                     Ok(Value::Lit(Lit::Num(l % r)))
        //                 }
        //                 _ => {
        //                     return Err(format!("Expected number, found {:?}", args).into());
        //                 }
        //             }
        //         }
        //     }),
        // );
        // env.borrow_mut().insert(
        //     ops.get(&InternedString::from("==")).unwrap().clone(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             Err(format!("Expected 2 args, found {}", args.len()).into())
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(l), Value::Lit(r)) => Ok(Value::Lit(Lit::Bool(l == r))),
        //                 (
        //                     Value::Lambda {
        //                         env: _,
        //                         params: p1,
        //                         body: b1,
        //                     },
        //                     Value::Lambda {
        //                         env: _,
        //                         params: p2,
        //                         body: b2,
        //                     },
        //                 ) => {
        //                     if p1.len() != p2.len() {
        //                         return Ok(Value::Lit(Lit::Bool(false)));
        //                     } else {
        //                         let mut p = true;
        //                         for (p1, p2) in p1.iter().zip(p2) {
        //                             p = *p1.inner() != *p2.inner();
        //                             break;
        //                         }
        //                         Ok(Value::Lit(Lit::Bool(b1 == b2 && p)))
        //                     }
        //                 }
        //                 (Value::Unit, Value::Unit) => Ok(Value::Lit(Lit::Bool(true))),
        //                 _ => Ok(Value::Lit(Lit::Bool(false))),
        //             }
        //         }
        //     }),
        // );
        // env.borrow_mut().insert(
        //     ops.get(&InternedString::from("!=")).unwrap().clone(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             Err(RuntimeError::ArityError(2, args.len()).into())
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(l), Value::Lit(r)) => Ok(Value::Lit(Lit::Bool(l != r))),
        //                 (
        //                     Value::Lambda {
        //                         env: _,
        //                         params: p1,
        //                         body: b1,
        //                     },
        //                     Value::Lambda {
        //                         env: _,
        //                         params: p2,
        //                         body: b2,
        //                     },
        //                 ) => {
        //                     if p1.len() != p2.len() {
        //                         return Ok(Value::Lit(Lit::Bool(true)));
        //                     } else {
        //                         let mut p = true;
        //                         for (p1, p2) in p1.iter().zip(p2) {
        //                             p = *p1.inner() != *p2.inner();
        //                             break;
        //                         }
        //                         Ok(Value::Lit(Lit::Bool(b1 != b2 || p)))
        //                     }
        //                 }
        //                 (Value::Unit, Value::Unit) => Ok(Value::Lit(Lit::Bool(false))),
        //                 _ => Ok(Value::Lit(Lit::Bool(true))),
        //             }
        //         }
        //     }),
        // );
        // env.borrow_mut().insert(
        //     ops.get(&InternedString::from("<")).unwrap().clone(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             Err(format!("Expected 2 args, found {}", args.len()).into())
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
        //                     Ok(Value::Lit(Lit::Bool(l < r)))
        //                 }
        //                 _ => {
        //                     return Err(format!("Expected number, found {:?}", args).into());
        //                 }
        //             }
        //         }
        //     }),
        // );
        // env.borrow_mut().insert(
        //     ops.get(&InternedString::from(">")).unwrap().clone(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             Err(format!("Expected 2 args, found {}", args.len()).into())
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
        //                     Ok(Value::Lit(Lit::Bool(l > r)))
        //                 }
        //                 _ => {
        //                     return Err(format!("Expected number, found {:?}", args).into());
        //                 }
        //             }
        //         }
        //     }),
        // );
        // env.borrow_mut().insert(
        //     ops.get(&InternedString::from("<=")).unwrap().clone(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             Err(format!("Expected 2 args, found {}", args.len()).into())
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
        //                     Ok(Value::Lit(Lit::Bool(l <= r)))
        //                 }
        //                 _ => {
        //                     return Err(format!("Expected number, found {:?}", args).into());
        //                 }
        //             }
        //         }
        //     }),
        // );
        // env.borrow_mut().insert(
        //     ops.get(&InternedString::from(">=")).unwrap().clone(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 2 {
        //             Err(format!("Expected 2 args, found {}", args.len()).into())
        //         } else {
        //             match (args.get(0).unwrap(), args.get(1).unwrap()) {
        //                 (Value::Lit(Lit::Num(l)), Value::Lit(Lit::Num(r))) => {
        //                     Ok(Value::Lit(Lit::Bool(l >= r)))
        //                 }
        //                 _ => {
        //                     return Err(format!("Expected number, found {:?}", args).into());
        //                 }
        //             }
        //         }
        //     }),
        // );
        // env.borrow_mut().insert(
        //     ops.get(&InternedString::from("print")).unwrap().clone(),
        //     Value::NativeFn(|args| {
        //         if args.len() != 1 {
        //             Err(format!("Expected 1 arg, found {}", args.len()).into())
        //         } else {
        //             println!("{}", args.get(0).unwrap());
        //             Ok(Value::Unit)
        //         }
        //     }),
        // );
        env
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Value {
        Lit(Lit),
        Lambda {
            env: Rc<RefCell<Env>>,
            param: Vec<UniqueId>,
            expr: Expr,
        },
        NativeFn(fn(Vec<Value>) -> RuntimeResult<Value>),
        Unit,
    }

    impl Display for Value {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Value::Lit(Lit::Num(num)) => write!(f, "{}", num),
                Value::Lit(Lit::Bool(b)) => write!(f, "{}", b),
                Value::Lit(Lit::String(s)) => write!(f, "{}", s),
                Value::Lambda { .. } => write!(f, "<lambda>"),
                Value::NativeFn { .. } => write!(f, "<native fn>"),
                Value::Unit => write!(f, "()"),
            }
        }
    }

    #[derive(Debug, Clone, PartialEq)]
    pub enum Lit {
        Num(Rational64),
        Bool(bool),
        String(InternedString),
    }

    impl Display for Lit {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self {
                Lit::Num(num) => write!(f, "{}", num),
                Lit::Bool(b) => write!(f, "{}", b),
                Lit::String(s) => write!(f, "{}", s),
            }
        }
    }

    // pub fn eval<'src>(
    //     src: &'src str,
    //     repl_src: &'src str,
    //     env: Rc<RefCell<Env>>,
    //     root: Root,
    // ) -> RuntimeResult<Value> {
    //     let mut val = Value::Unit;
    //     for decl in root.decls() {
    //         match decl.kind() {
    //             Item::Expr(expr) => {
    //                 val = eval_expr(src, repl_src, env.clone(), Node::new(expr, item.span()))?;
    //             }
    //             Item::Def { pat, expr, .. } => {
    //                 let value = eval_expr(src, repl_src, env.clone(), expr)?;
    //                 let ds = destructure_pattern(env.clone(), pat, Node::new(value, item.span()));
    //                 // env.borrow_mut().insert(name.inner().clone(), value);
    //                 val = Value::Unit;
    //             }
    //         }
    //     }
    //     Ok(val)
    // }

    // fn eval_expr<'src>(
    //     src: &'src str,
    //     repl_src: &'src str,
    //     mut env: Rc<RefCell<Env>>,
    //     mut expr: Expr,
    // ) -> RuntimeResult<Value> {
    //     let val: Value;
    //     'tco: loop {
    //         val = match expr.kind() {
    //             ExprKind::Lit { lit, .. } => match lit {
    //                 rename::Lit::Num(num) => Value::Lit(Lit::Num(num.clone())),
    //                 rename::Lit::Bool(b) => Value::Lit(Lit::Bool(b)),
    //                 rename::Lit::String(s) => Value::Lit(Lit::String(s.clone())),
    //             },
    //             Expr::Ident { name, .. } => {
    //                 if let Some(value) = env.borrow().get(&name) {
    //                     value
    //                 } else {
    //                     return Err(
    //                         format!("Identifier '{}' not found", repl_src[name.span()].trim()).into(),
    //                     );
    //                 }
    //             }
    //             Expr::Lambda { params, body, .. } => Value::Lambda {
    //                 env: env.clone(),
    //                 params,
    //                 body,
    //             },
    //             Expr::Apply { fun, args, .. } => {
    //                 let fun = eval_expr(src, repl_src, env.clone(), fun)?;
    //                 // println!("fun: {:?}", fun);
    //                 match fun {
    //                     Value::Lambda {
    //                         env: lam_env,
    //                         params,
    //                         body,
    //                     } => {
    //                         let arg_env = Env::new_with_parent(lam_env.clone());
    //                         for (p, arg) in params.iter().zip(args) {
    //                             if !destructure_pattern(
    //                                 arg_env.clone(),
    //                                 p.clone(),
    //                                 Node::new(
    //                                     eval_expr(src, repl_src, env.clone(), arg.clone())?,
    //                                     expr.span(),
    //                                 ),
    //                             ) {
    //                                 return Err(format!("Could not destructure pattern").into());
    //                             }
    //                         }

    //                         expr = body;
    //                         env = arg_env;
    //                         continue 'tco;
    //                     }
    //                     Value::NativeFn(fun) => {
    //                         let mut new_args = Vec::new();
    //                         for arg in args {
    //                             new_args.push(eval_expr(src, repl_src, env.clone(), arg)?);
    //                         }
    //                         fun(new_args)?
    //                     }
    //                     _ => {
    //                         return Err(format!("Expected lambda, found {:?}", fun).into());
    //                     }
    //                 }
    //             }
    //             Expr::Let {
    //                 pat,
    //                 expr: let_expr,
    //                 body,
    //                 ..
    //             } => {
    //                 let value = eval_expr(src, repl_src, env.clone(), let_expr.clone())?;
    //                 if destructure_pattern(env.clone(), pat, Node::new(value, let_expr.span())) {
    //                     expr = body;
    //                     continue 'tco;
    //                 } else {
    //                     return Err(format!("Could not destructure pattern").into());
    //                 }
    //             }
    //             Expr::Fn {
    //                 name,
    //                 params,
    //                 expr,
    //                 body,
    //                 ty,
    //             } => {
    //                 let value = Value::Lambda {
    //                     env: Env::new_with_parent(env.clone()),
    //                     params,
    //                     body: expr.clone(),
    //                 };
    //                 env.borrow_mut().insert(name.inner().clone(), value);
    //                 eval_expr(src, repl_src, env, body)?
    //             }
    //             Expr::Match {
    //                 expr: mexpr,
    //                 cases,
    //                 ty,
    //             } => {
    //                 let val = eval_expr(src, repl_src, env.clone(), mexpr.clone())?;
    //                 let match_env = Env::new_with_parent(env.clone());
    //                 for case in cases {
    //                     if destructure_pattern(
    //                         match_env.clone(),
    //                         case.pattern.clone(),
    //                         Node::new(val.clone(), mexpr.span()),
    //                     ) {
    //                         env = match_env;
    //                         expr = case.expr.clone();
    //                         continue 'tco;
    //                     } else {
    //                         continue;
    //                     }
    //                 }
    //                 return Err(format!("No matching pattern found for {:?}", val).into());
    //             }
    //             Expr::If {
    //                 cond, then, else_, ..
    //             } => {
    //                 let cond = eval_expr(src, repl_src, env.clone(), cond)?;
    //                 match cond {
    //                     Value::Lit(Lit::Bool(true)) => {
    //                         expr = then;
    //                         continue 'tco;
    //                     }
    //                     Value::Lit(Lit::Bool(false)) => {
    //                         expr = else_;
    //                         continue 'tco;
    //                     }
    //                     _ => {
    //                         return Err(format!("Expected bool, found {:?}", cond).into());
    //                     }
    //                 }
    //             }
    //             Expr::Unit => Value::Unit,
    //         };

    //         break 'tco;
    //     }
    //     Ok(val)
    // }

    // fn destructure_pattern(env: Rc<RefCell<Env>>, pat: Node<rename::Pattern>, val: Node<Value>) -> bool {
    //     match pat.inner().clone() {
    //         rename::Pattern::Lit(l) => {
    //             if let Value::Lit(v) = val.inner() {
    //                 l == *v
    //             } else {
    //                 false
    //             }
    //         }
    //         rename::Pattern::Ident(name) => {
    //             env.borrow_mut().insert(name, val.inner().clone());
    //             true
    //         }
    //         rename::Pattern::Wildcard => true,
    //         rename::Pattern::Unit => Value::Unit == val.inner().clone(),
    //     }
    // }
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
