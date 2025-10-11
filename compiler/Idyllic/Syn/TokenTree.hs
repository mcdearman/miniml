module Idyllic.Syn.TokenTree (TokenTree (..), Delim (..), Punct (..)) where

import Idyllic.Utils.Span (Span)

data TokenTree = TokenTree
  { tokenTreeKind :: TokenTreeKind,
    tokenTreeSpan :: Span,
    tokenTreeOrigin :: Origin
  }
  deriving (Show, Eq, Ord)

data TokenTreeKind
  = TokenTreeError
  | TokenTreeGroup Delim [TokenTree]
  | TokenTreeIdent Ident
  | TokenTreePunct Punct
  | TokenTreeLiteral Literal
  deriving (Show, Eq, Ord)

data Origin = Original | SyntheticLayout | SyntheticMacro
  deriving (Eq, Show, Ord)

data Delim
  = DelimParen
  | DelimBrace
  | DelimBracket
  deriving (Show, Eq, Ord)

data Punct
  = PunctBang
  | PunctBackSlash
  | PunctColon
  | PunctSemi
  | PunctComma
  | PunctPeriod
  | PunctEq
  | PunctLArrow
  | PunctRArrow
  | PunctLFatArrow
  | PunctBar
  | PunctUnderscore
  deriving (Show, Eq, Ord)

data Ident
  = IdentError
  | IdentComment
  | IdentUppercaseIdent
  | IdentLowercaseIdent
  | IdentOpIdent
  | IdentConOpIdent
  deriving (Show, Eq, Ord)

data Literal
  = LitInt
  | LitString
  | LitChar
  deriving (Show, Eq, Ord)