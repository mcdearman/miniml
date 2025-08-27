module MMC.Syn.TokenTree (LTokenTree, TokenTree (..), LDelim, Delim (..), LAtom, Atom (..)) where

import Data.Text (Text)
import MMC.Common (Located)

type LTokenTree = Located TokenTree

data TokenTree
  = TokTreeAtom (LAtom)
  | TokTreeDelim LDelim [TokenTree]
  deriving (Show, Eq, Ord)

type LDelim = Located Delim

data Delim
  = DelimParen
  | DelimBrace
  | DelimBracket
  deriving (Show, Eq, Ord)

type LAtom = Located Atom

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
