module MMC.Syn.Token (Token (..), TokenKind (..)) where

import Data.Text (Text)
import MMC.Common (Loc)

data Token = Token
  { tokenKind :: TokenKind,
    tokenLoc :: Loc
  }
  deriving (Show, Eq, Ord)

data TokenKind
  = TokenKindError
  | TokenKindWhitespace
  | TokenKindNewline
  | TokenKindComment
  | TokenKindUpperCaseIdent Text
  | TokenKindLowerCaseIdent Text
  | TokenKindOpIdent Text
  | TokenKindConOpIdent Text
  | TokenKindInt Integer
  | TokenKindString Text
  | TokenKindChar Char
  | TokenKindLParen
  | TokenKindRParen
  | TokenKindLBrace
  | TokenKindRBrace
  | TokenKindLBracket
  | TokenKindRBracket
  | TokenKindBang
  | TokenKindBackSlash
  | TokenKindColon
  | TokenKindSemi
  | TokenKindComma
  | TokenKindPeriod
  | TokenKindEq
  | TokenKindLArrow
  | TokenKindRArrow
  | TokenKindLFatArrow
  | TokenKindBar
  | TokenKindUnderscore
  | TokenKindModule
  | TokenKindImport
  | TokenKindAs
  | TokenKindLet
  | TokenKindIn
  | TokenKindWhere
  | TokenKindIf
  | TokenKindThen
  | TokenKindElse
  | TokenKindMatch
  | TokenKindWith
  | TokenKindRecord
  | TokenKindData
  | TokenKindType
  | TokenKindClass
  | TokenKindInstance
  | TokenKindDo
  deriving (Show, Eq, Ord)
