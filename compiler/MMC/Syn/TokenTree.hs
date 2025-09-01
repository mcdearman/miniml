module MMC.Syn.TokenTree (TokenTree (..), Delim (..), Atom (..)) where

import Data.Text (Text)

data TokenTree
  = TokTreeAtom Atom
  | TokTreeDelim Delim [TokenTree]
  deriving (Show, Eq, Ord)

data Origin = Original | SyntheticLayout | SyntheticMacro
  deriving (Eq, Show)

data Delim
  = DelimParen
  | DelimBrace
  | DelimBracket
  deriving (Show, Eq, Ord)

data Atom
  = AtomError
  | AtomEOF
  | AtomUpperCaseIdent Text
  | AtomLowerCaseIdent Text
  | AtomOpIdent Text
  | AtomConOpIdent Text
  | AtomInt Integer
  | AtomString Text
  | AtomChar Char
  | AtomBang
  | AtomPlus
  | AtomMinus
  | AtomStar
  | AtomSlash
  | AtomBackSlash
  | AtomPercent
  | AtomColon
  | AtomSemi
  | AtomComma
  | AtomPeriod
  | AtomEq
  | AtomRArrow
  | AtomLArrow
  | AtomLFatArrow
  | AtomBar
  | AtomUnderscore
  | AtomModule
  | AtomImport
  | AtomAs
  | AtomLet
  | AtomIn
  | AtomWhere
  | AtomIf
  | AtomThen
  | AtomElse
  | AtomMatch
  | AtomWith
  | AtomRecord
  | AtomData
  | AtomType
  | AtomClass
  | AtomInstance
  | AtomDo
  deriving (Show, Eq, Ord)
